{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Data.Attoparsec.ByteString.Lazy (parse)
import Data.Attoparsec.ByteString.Char8 hiding (parse)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.UTF8 (toString)
import Data.Char (isPrint)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy
import Prelude hiding (getContents)

-- TODO handle exceptions

data HTTPRequest = HTTPRequest {
        qMethod :: String,
        qPath :: String,
        qHeaders :: [(String, String)]
    } deriving (Show, Read, Eq, Ord)

main :: IO ()
main = do
    addr <- inet_addr "0.0.0.0"
    s <- socket AF_INET Stream defaultProtocol
    bind s $ SockAddrInet 3000 addr
    listen s 1
    putStrLn "Listening"
    forever $ do
        (c, remote) <- accept s
        forkIO $ worker c remote

worker :: Socket -> SockAddr -> IO ()
worker c remote = do
    putStrLn $ "Connection from: " ++ (show remote)
    req <- getContents c
    BL.putStrLn $ BL.take 100 req
    close c

-- TODO helpers like skipSpace = skipWhile (== ' ')
method = choice $ map stringCI [ "OPTIONS", "GET", "HEAD", "POST", "PUT", "DELETE", "TRACE" ]
uri = many' (satisfy (not . isSpace)) -- TODO actually only ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~:/?#[]@!$&'()*+,;= should be valid (RFC 3986) so we want an efficient Char -> Bool that is True for these chars
httpVersion = string "HTTP/1.1" <|> string "HTTP/1.0"
header = do
    k <- many' (satisfy (\x -> not (isSpace x) && x /= ':'))
    char ':'
    skipWhile (== ' ')
    v <- many' (satisfy (not . isSpace))
    string "\r\n"
    return (k, v)

httpRequest = do
    m <- method
    skipWhile (== ' ')
    u <- uri
    skipWhile (== ' ')
    _ <- httpVersion
    string "\r\n"
    hs <- many' header
    return $ HTTPRequest (toString m) u hs
