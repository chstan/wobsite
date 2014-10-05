import Network
import Control.Concurrent
import System.IO
import ResponseRequest
import App
import Middleware

appWithMiddleware :: Request -> IO Response
appWithMiddleware = (application . breakPath)

respond :: Request -> Handle -> IO ()
respond request handle = do
  response <- appWithMiddleware request
  hPutStr handle $ (show response)

parseRequestType :: String -> RequestType
parseRequestType s = case s of
  "GET" -> GET
  "POST" -> POST
  "PUT" -> PUT
  "DELETE" -> DELETE

parseOptionsHelper :: [String] -> [(String, String)] -> [(String, String)]
parseOptionsHelper [] acc = acc
parseOptionsHelper (f:xs) acc
  | (length (words f)) < 2 = acc
  | otherwise = parseOptionsHelper xs (acc ++ [builtOption])
 where
  option = reverse . tail . reverse . head . words $ f
  value = unwords . tail . words $ f
  builtOption = (option, value)



parseOptions :: [String] -> [(String, String)]
parseOptions l = parseOptionsHelper l []

parseRequest :: [String] -> Request
parseRequest l = case (words (head l)) of
  -- Should really do some additional validation of the request
  [recvType, recvPath, recvMethod] ->
    Request { rtype = (parseRequestType recvType),
              path = RawPath recvPath,
              options = (parseOptions (tail l))}

handleAccept :: Handle -> String -> IO ()
handleAccept handle _ = do
  request <- fmap (parseRequest . lines) (hGetContents handle)
  respond request handle
  return ()

welcomeMessage :: (Show a, Num a) => a -> IO ()
welcomeMessage pn = do
  putStrLn "Serving is starting up..."
  putStrLn $ "Preparing to serve requests on " ++ (show pn)

acceptingPort :: (Num a) => a
acceptingPort = 3000

main :: IO ()
main = withSocketsDo $ do
    welcomeMessage (acceptingPort :: Integer)
    sock <- listenOn $ PortNumber acceptingPort
    loop sock

loop :: Socket -> IO ()
loop sock = do
   (handle,hostname,_) <- accept sock
   forkIO $ daemon handle hostname
   loop sock
  where
   daemon handle hostname = do
       handleAccept handle hostname
       hFlush handle
       hClose handle
