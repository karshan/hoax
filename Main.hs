{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Applicative              ((<$>), (<|>))
import           Control.Concurrent               (forkIO)
import           Control.Exception                (SomeException, catch, try)
import           Control.Monad                    (forever)
import           Data.Attoparsec.ByteString       (eitherResult, maybeResult,
                                                   parse, parseOnly)
import           Data.Attoparsec.ByteString.Char8 hiding (eitherResult,
                                                   maybeResult, parse,
                                                   parseOnly, try)
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.UTF8             as B (toString)
import           Data.Char                        (isPrint, ord)
import           Data.Function                    ((&))
import           Data.Maybe                       (fromMaybe, listToMaybe)
import           Data.String                      (IsString (..))
import           GHC.Word                         (Word8)
import           Network.Socket                   hiding (recv, recvFrom, send,
                                                   sendTo)
import           Network.Socket.ByteString
import           Network.Socks5                   ()
import           Prelude                          hiding (getContents, takeWhile)
import           System.Environment               (getArgs)
import           System.Exit                      (exitSuccess)
import           System.Posix.Signals             (Handler (Catch),
                                                   installHandler,
                                                   keyboardSignal)
import           System.Random                    (randomIO)

-- TODO handle exceptions

instance IsString [Word8] where
    fromString = map (fromIntegral . ord)

data HTTPRequest = HTTPRequest {
        qMethod  :: B.ByteString,
        qPath    :: B.ByteString,
        qHeaders :: [(B.ByteString, B.ByteString)],
        qBody    :: B.ByteString
    } deriving (Show, Read, Eq, Ord)

setPort :: PortNumber -> SockAddr -> SockAddr
setPort p (SockAddrInet _ h) = SockAddrInet p h
setPort p (SockAddrInet6 _ f h s) = SockAddrInet6 p f h s
setPort _ _ = error "wtf seriously, getaddrinfo returned a unix socket !?"

connect' :: ByteString -> Integer -> IO (Maybe Socket)
connect' host port = do
    out_socket <- socket AF_INET Stream defaultProtocol
    msockaddr <- fmap (setPort (fromInteger port) . addrAddress) . listToMaybe . filter ((/= AF_INET6) . addrFamily) <$>
        getAddrInfo Nothing (Just . B.toString $ host) Nothing
    msockaddr & maybe (putStrLn "getAddrInfo failed" >> return Nothing) (\sockaddr -> do
        e <- try (connect out_socket sockaddr)
        either (\(_ :: SomeException) -> putStrLn "connect failed" >> return Nothing) (const (return (Just out_socket))) e)

errorPrint :: (Show a) => String -> Maybe a -> IO ()
errorPrint msg m_x = putStrLn $ "ERROR: " ++ msg ++ maybe "" (\x -> "<" ++ show x ++ ">") m_x

debug :: (Show a) => String -> Maybe a -> IO ()
-- debug = const $ return ()
debug msg m_x = putStrLn $ "DEBUG: " ++ msg ++ maybe "" (\x -> "<" ++ show x ++ ">") m_x

main :: IO ()
main = do
    port <- (fromInteger . fromMaybe 3000 . fmap read . listToMaybe) <$> getArgs
    addr <- inet_addr "0.0.0.0"
    s <- socket AF_INET Stream defaultProtocol
    setSocketOption s ReuseAddr 1
    installHandler keyboardSignal (Catch $ close s >> exitSuccess) Nothing
    bind s $ SockAddrInet port addr
    listen s 10
    putStrLn $ "Listening on port " ++ show port
    forever $ do
        (c, remote) <- accept s
        forkIO $ worker c remote

worker :: Socket -> SockAddr -> IO ()
worker c remote = do
    reqraw <- recv c (32*1024)
    either (\x -> errorPrint "parseOnly httpRequest" (Just (x, reqraw, remote))) (\req -> do
        forward req c) $ parseOnly httpRequest reqraw

changePort (SockAddrInet _ h) p      = SockAddrInet p h
changePort (SockAddrInet6 _ a b c) p = SockAddrInet6 p a b c

--socksConnect' sh sp h p = socksConnect (defaultSocksConf sh sp) (SocksAddress (SocksAddrDomainName h) (fromInteger p))

forward :: HTTPRequest -> Socket -> IO ()
forward req@(HTTPRequest { qMethod = "CONNECT" }) c = do
    either (errorPrint "parseOnly connectHost" . Just) (\(h, p) -> do
        debug "CONNECT to" (Just (h, p))
        mSock <- connect' h (read (B.toString p)) -- this read won't fail because p is guaranteed to be a string containing only digits by the connectHost parser
        maybe (close c >> errorPrint "connect failed" (Just req))
            (\sock -> do
                sendAll c connect200response
                _ <- forkIO $ doCopy c sock
                _ <- forkIO $ doCopy sock c
                return ())
            mSock)
        (parseOnly connectHost (qPath req))
        where
            connect200response = "HTTP/1.1 200 OK\r\nConnection Established\r\n\r\n"
forward req c = errorPrint "not-CONNECT" (Just (qMethod req, qPath req)) >> close c

doCopy :: Socket -> Socket -> IO ()
doCopy from to = do
    r <- (recv from (32*1024)) `catch` (\(e :: SomeException) -> close from >> return "") -- Magic Numbers from golang
    if B.null r then close from
    else (sendAll to r >> doCopy from to) `catch` (\(e :: SomeException) -> close from)

{- TODO helpers
    skipSpace' = skipWhile (== ' ')
    manyBs = fmap fromString . many'
-}

connectHost :: Parser (B.ByteString, B.ByteString)
connectHost = do
    h <- takeWhile (/= ':')
    char ':'
    p <- takeWhile isDigit
    return (h, p)
        where
            isDigit w = w >= '0' && w <= '9'

method :: Parser B.ByteString
method = choice $ map stringCI [ "CONNECT", "OPTIONS", "GET", "HEAD", "POST", "PUT", "DELETE", "TRACE" ]

uri :: Parser B.ByteString
uri = takeWhile (not . isSpace) -- TODO actually only ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~:/?#[]@!$&'()*+,;= should be valid (RFC 3986) so we want an efficient Char -> Bool that is True for these chars

httpVersion :: Parser B.ByteString
httpVersion = string "HTTP/1.1" <|> string "HTTP/1.0"

-- Note this will miss headers that do not end with \r\n.
header :: Parser (B.ByteString, B.ByteString)
header = do
    k <- takeWhile (\x -> not (isSpace x) && x /= ':')
    char ':'
    skipWhile (== ' ')
    v <- takeWhile (\x -> x /= '\r' && x /= '\n') -- this is not precise I want to takeWhile 2 bytes aren't equal to "\r\n" TODO
    string "\r\n" <|> string "\n"
    return (k, v)

httpRequest :: Parser HTTPRequest
httpRequest = do
    m <- method
    skipWhile (== ' ')
    u <- uri
    skipWhile (== ' ')
    _ <- httpVersion
    string "\r\n" <|> string "\n"
    hs <- many' header
    string "\r\n" <|> string "\n"
    b <- takeByteString -- TODO takeLazyByteString ???
    return $ HTTPRequest m u hs b
