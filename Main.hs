{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
import           Control.Applicative              ((<$>), (<|>))
import           Control.Concurrent.Async         (async, wait, waitBoth)
import           Control.Exception                (SomeException, bracket,
                                                   catch, try)
import           Control.Monad                    (forever)
import           Data.Attoparsec.ByteString       (eitherResult, maybeResult,
                                                   parse, parseOnly)
import           Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P
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
import           Network.Socket.ByteString        (recv, recvFrom, send,
                                                   sendAll, sendTo)
import           Prelude                          (String)
import           Protolude
import           System.Environment               (getArgs)
import           System.Exit                      (exitSuccess)
import           System.Posix.Signals             (Handler (Catch),
                                                   installHandler,
                                                   keyboardSignal)
import           System.Random                    (randomIO)

instance IsString [Word8] where
    fromString = map (fromIntegral . ord)

data HTTPRequest = HTTPRequest {
        qMethod  :: B.ByteString,
        qPath    :: B.ByteString,
        qHeaders :: [(B.ByteString, B.ByteString)],
        qBody    :: B.ByteString
    } deriving (Show, Read, Eq, Ord)

setPort :: PortNumber -> SockAddr -> Either Text SockAddr
setPort p (SockAddrInet _ h) = Right $ SockAddrInet p h
setPort p (SockAddrInet6 _ f h s) = Right $ SockAddrInet6 p f h s
setPort _ _ = Left "wtf seriously, getaddrinfo returned a unix socket !?"

withSocket ::  Family -> SocketType -> ProtocolNumber -> (Socket -> IO a) -> IO a
withSocket family sktType protocol =
    bracket (socket family sktType protocol)  -- acquire resource
            (\skt -> shutdown skt ShutdownBoth >> close skt) -- release

monadBind :: Monad m => (a -> m b) -> m a -> m b
monadBind = (=<<)

-- TODO return EitherT IO instead ?
getAddrInfo' :: StringConv host String => host -> Integer -> IO (Either Text SockAddr)
getAddrInfo' host port =
    (setPort (fromInteger port) . addrAddress <=< maybeToEither "getAddrInfo no results" . head) .
    filter ((/= AF_INET6) . addrFamily) <$>
        getAddrInfo Nothing (Just . toS $ host) Nothing

withConnect :: ByteString -> Integer -> (Socket -> IO a) -> IO (Either Text a)
withConnect host port cont =
    withSocket AF_INET Stream defaultProtocol $ \s -> do
        getAddrInfo' host port >>=
            either
                (return . Left)
                (\sockaddr -> do
                    connect s sockaddr
                    fmap Right $ cont s)

logPrint :: (Show a) => Text -> Text -> Maybe a -> IO ()
logPrint prefix msg m_x = putStrLn $ prefix <> ": " <> msg <> maybe "" (\x -> "<" <> show x <> ">") m_x

debug :: (Show a) => Text -> Maybe a -> IO ()
debug = logPrint "debug"

err :: (Show a) => Text -> Maybe a -> IO ()
err = logPrint "error"

-- TODO OptParse Applicative stack template
main :: IO ()
main = do
    port <- (fromInteger . fromMaybe 3000 . monadBind readMaybe . head) <$> getArgs
    addr <- inet_addr "0.0.0.0"
    withSocket AF_INET Stream defaultProtocol $ \s -> do
        setSocketOption s ReuseAddr 1
        installHandler keyboardSignal (Catch $ close s >> exitSuccess) Nothing
        bind s $ SockAddrInet port addr
        listen s 10
        debug "Listening on port " (Just port)
        forever $ do
            (c, remote) <- accept s
            async (worker c remote)

worker :: Socket -> SockAddr -> IO ()
worker c remote = do
    bracket_ (return ())
        (shutdown c ShutdownBoth >> close c)
        (do
            reqraw <- recv c (32*1024)
            either (\x -> err "parseOnly httpRequest" (Just (x, reqraw, remote))) (\req -> do
                forward req c) $ parseOnly httpRequest reqraw)

changePort (SockAddrInet _ h) p      = SockAddrInet p h
changePort (SockAddrInet6 _ a b c) p = SockAddrInet6 p a b c

--socksConnect' sh sp h p = socksConnect (defaultSocksConf sh sp) (SocksAddress (SocksAddrDomainName h) (fromInteger p))

forward :: HTTPRequest -> Socket -> IO ()
forward req@(HTTPRequest { qMethod = "CONNECT" }) c = do
    either (err "parseOnly connectHost" . Just) (\(h, p) -> do
        debug "CONNECT to" (Just (h, p))
        maybe
            (err "failed to read port" (Just p))
            (\portInt -> do
                void $ withConnect h portInt $ \s -> do
                    sendAll c connect200response
                    a1 <- async $ doCopy c s
                    a2 <- async $ doCopy s c
                    waitBoth a1 a2
                    return ())
            (readMaybe (toS p)))
        (parseOnly connectHost (qPath req))
        where
            connect200response = "HTTP/1.1 200 OK\r\nConnection Established\r\n\r\n"
forward req c = err "not-CONNECT" (Just (qMethod req, qPath req))

doCopy :: Socket -> Socket -> IO ()
doCopy from to = do
    r <- (recv from (32*1024)) -- Magic Numbers from golang
    if B.null r then return ()
    else (sendAll to r >> doCopy from to)

{- TODO helpers
    skipSpace' = skipWhile (== ' ')
    manyBs = fmap fromString . many'
-}

connectHost :: Parser (B.ByteString, B.ByteString)
connectHost = do
    h <- P.takeWhile (/= ':')
    P.char ':'
    p <- P.takeWhile isDigit
    return (h, p)
        where
            isDigit w = w >= '0' && w <= '9'

method :: Parser B.ByteString
method = P.choice $ map P.stringCI [ "CONNECT", "OPTIONS", "GET", "HEAD", "POST", "PUT", "DELETE", "TRACE" ]

uri :: Parser B.ByteString
uri = P.takeWhile (not . P.isSpace) -- TODO actually only ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~:/?#[]@!$&'()*+,;= should be valid (RFC 3986) so we want an efficient Char -> Bool that is True for these chars

httpVersion :: Parser B.ByteString
httpVersion = P.string "HTTP/1.1" <|> P.string "HTTP/1.0"

-- Note this will miss headers that do not end with \r\n.
header :: Parser (B.ByteString, B.ByteString)
header = do
    k <- P.takeWhile (\x -> not (P.isSpace x) && x /= ':')
    P.char ':'
    P.skipWhile (== ' ')
    v <- P.takeWhile (\x -> x /= '\r' && x /= '\n') -- this is not precise I want to takeWhile 2 bytes aren't equal to "\r\n" TODO
    P.string "\r\n" <|> P.string "\n"
    return (k, v)

httpRequest :: Parser HTTPRequest
httpRequest = do
    m <- method
    P.skipWhile (== ' ')
    u <- uri
    P.skipWhile (== ' ')
    _ <- httpVersion
    P.string "\r\n" <|> P.string "\n"
    hs <- P.many' header
    P.string "\r\n" <|> P.string "\n"
    b <- P.takeByteString -- TODO takeLazyByteString ???
    return $ HTTPRequest m u hs b
