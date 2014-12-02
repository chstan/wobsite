{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as BSL8

import Control.Monad (liftM)

import qualified Data.Map as Map
import Network hiding (accept)
import Network.Socket
import Network.Socket.ByteString.Lazy as NSBL (getContents, sendAll)
import Control.Concurrent
import Control.Concurrent.STM
import Data.List.Split (splitOn)

import Data.Chess (ChessEngineHandle)
import Data.Config
import ResponseRequest
import App
import Middleware


appWithMiddleware :: Request -> IO Response
appWithMiddleware req =
  ((liftM $ compressResponse req) . application . breakPath) req

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

parseOptions :: [BSL8.ByteString] -> Map.Map String String
parseOptions l = Map.fromList $ parseOptionsHelper l []

parseRequest :: ConfigurationType -> [BSL8.ByteString] -> Request
parseRequest config l = case (BSL8.words (head l)) of
  -- Should really do some additional validation of the request
  [recvType, recvPath, _] ->
    Request (parseRequestType recvType)
            (RawPath $ BSL8.unpack recvPath)
             config
            (parseOptions (tail l))

connectionAccept :: Socket -> ConfigurationType -> IO ()
connectionAccept c config = do
  request <- fmap ((parseRequest config) . BSL8.lines) (NSBL.getContents c)
  respond request c
  return ()

welcomeMessage :: (Show a, Num a) => a -> IO ()
welcomeMessage pn = do
  putStrLn "Server is starting up..."
  putStrLn $ "Preparing to serve requests on " ++ (show pn)

main :: IO ()
main = withSocketsDo $ do
  contents <- fmap (filter (not . null)) $ fmap (splitOn "\n") $ readFile "configuration"
  case envAndPortFromLines contents of
   (Just e, Just pn) -> do
     sharedEngineHandles <- atomically $ newTVar $ Map.fromList []
     welcomeMessage pn
     sock <- listenOn $ PortNumber $ fromIntegral pn
     let config = ConfigurationType sharedEngineHandles e
     loop sock config

   -- Invalid configuration
   _ -> do
     putStrLn "Invalid server configuration file."
     return ()


loop :: Socket -> ConfigurationType -> IO ()
loop sock config = do
   (connection,_) <- accept sock
   forkIO $ daemon connection config
   loop sock config
  where
   daemon c conf = do
       connectionAccept c conf
       Network.Socket.sClose c
