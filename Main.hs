{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as BSL8

import Network hiding (accept)
import Network.Socket
--import Network.Socket.ByteString (send)
import Network.Socket.ByteString.Lazy as NSBL (getContents, sendAll)
import Control.Concurrent
import System.IO hiding (getContents)
import ResponseRequest
import App
import Middleware

appWithMiddleware :: Request -> IO Response
appWithMiddleware = (application . breakPath)

respond :: Request -> Socket -> IO ()
respond req c  = do
  response <- appWithMiddleware req
  NSBL.sendAll c $ BSL8.pack $ show response

parseRequestType :: BSL8.ByteString -> RequestType
parseRequestType s = case s of
  "GET" -> GET
  "POST" -> POST
  "PUT" -> PUT
  "DELETE" -> DELETE

parseOptionsHelper :: [BSL8.ByteString] -> [(String, String)] -> [(String, String)]
parseOptionsHelper [] acc = acc
parseOptionsHelper (f:xs) acc
  | (length (BSL8.words f)) < 2 = acc
  | otherwise = parseOptionsHelper xs (acc ++ [builtOption])
 where
  option = reverse . tail . reverse . head . words $ BSL8.unpack f
  value = unwords . tail . words $ BSL8.unpack f
  builtOption = (option, value)

parseOptions :: [BSL8.ByteString] -> [(String, String)]
parseOptions l = parseOptionsHelper l []

parseRequest :: [BSL8.ByteString] -> Request
parseRequest l = case (BSL8.words (head l)) of
  -- Should really do some additional validation of the request
  [recvType, recvPath, recvMethod] ->
    Request { rtype = (parseRequestType recvType),
              path = RawPath $ BSL8.unpack recvPath,
              options = (parseOptions (tail l))}

connectionAccept :: Socket -> IO ()
connectionAccept c = do
  request <- fmap (parseRequest . BSL8.lines) (NSBL.getContents c)
  respond request c
  return ()

welcomeMessage :: (Show a, Num a) => a -> IO ()
welcomeMessage pn = do
  putStrLn "Server is starting up..."
  putStrLn $ "Preparing to serve requests on " ++ (show pn)

main :: IO ()
main = withSocketsDo $ do
  contents <- readFile "configuration"
  welcomeMessage (read contents :: Int)
  sock <- listenOn $ PortNumber $ fromIntegral
          $ (read :: String -> Integer) contents
  loop sock

loop :: Socket -> IO ()
loop sock = do
   (connection,_) <- accept sock
   forkIO $ daemon connection
   loop sock
  where
   daemon c = do
       connectionAccept c
       Network.Socket.sClose c
