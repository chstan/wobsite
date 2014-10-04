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
handleAccept handle hostname = do
  request <- fmap (parseRequest . lines) (hGetContents handle)
  respond request handle
  return ()

main = withSocketsDo $ do
    sock <- listenOn $ PortNumber 3000
    loop sock

loop sock = do
   (handle,hostname,port) <- accept sock
   forkIO $ body handle hostname
   loop sock
  where
   body handle hostname = do
       handleAccept handle hostname
       hFlush handle
       hClose handle
