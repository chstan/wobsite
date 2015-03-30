{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString.Lazy.Char8 as BSL8

import Control.Monad (liftM)

import qualified Data.Map as Map
import Network hiding (accept)
import Network.Socket
import System.IO (hClose)
import Control.Exception (Exception, SomeException, finally, catch)
import Network.Socket.ByteString.Lazy as NSBL (getContents, sendAll, recv)
import Control.Concurrent
import Control.Concurrent.Async (race)
import Control.Concurrent.STM
import Data.List.Split (splitOn)

import Data.Config
import ResponseRequest
import App
import Middleware


appWithMiddleware :: Request -> IO Response
appWithMiddleware req =
  ((fmap cacheImages) . (liftM $ compressResponse req) . application .
   breakPath . breakQueryString) req

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

parseRequest :: ConfigurationType -> [BSL8.ByteString] -> Maybe Request
parseRequest _ [] = Nothing
parseRequest config l = case (BSL8.words (head l)) of
  -- Should really do some additional validation of the request
  [recvType, recvPath, _] ->
    Just $ Request (parseRequestType recvType)
           (RawPath $ BSL8.unpack recvPath)
            config
           (Map.fromList [])
           (parseOptions (tail l))
  _ -> Nothing

connectionAccept :: Socket -> ConfigurationType -> IO ()
connectionAccept c config = do
  readable <- isReadable c
  case readable of
   False -> return ()
   True -> do
     d <- race
        (do
            threadDelay 1000000
            return ())
        (do
            contents <- NSBL.recv c 16392
            return contents)
     case d of
      Left () -> do
        return () -- close the conn
      Right bytes -> do
        let mrequest = ((parseRequest config) . BSL8.lines) bytes
        case mrequest of
         Nothing -> return ()
         Just request -> do
           respond request c
           case Map.lookup "Connection" $ options request of
            Just "keep-alive" -> connectionAccept c config
            Nothing           -> connectionAccept c config
            Just _            -> return ()

welcomeMessage :: (Show a, Num a) => a -> IO ()
welcomeMessage pn = do
  putStrLn "Server is starting up..."
  putStrLn $ "Preparing to serve requests on " ++ (show pn)

main :: IO ()
main = withSocketsDo $ do
  contents <- fmap (filter (not . null)) $ fmap (splitOn "\n") $ readFile "config/configuration"
  case envAndPortFromLines contents of
   (Just e, Just pn) -> do
     sharedEngineHandles <- atomically $ newTVar $ Map.fromList []
     welcomeMessage pn
     sock <- listenOn $ PortNumber $ fromIntegral pn
     setSocketOption sock KeepAlive 1
     setSocketOption sock ReuseAddr 1
     let config = ConfigurationType sharedEngineHandles e
     loop sock config

   -- Invalid configuration
   _ -> do
     putStrLn "Invalid server configuration file."
     return ()


loop :: Socket -> ConfigurationType -> IO ()
loop sock config = do
   (connection,_) <- accept sock
   forkIO (catch
            (finally
             (connectionAccept connection config)
             (Network.Socket.sClose connection)
            )
            (\x -> putStrLn $ show (x :: SomeException))
          )
   loop sock config
