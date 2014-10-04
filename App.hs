module App
       (application) where

import ResponseRequest
import Routes
import Aux

fourOhFourResponse :: String -> ContentDescType -> Response
fourOhFourResponse b c = Response { version = "HTTP/1.1",
                                  statuscode = 404,
                                  contentDescription = c,
                                  body = b }

okayResponse :: String -> ContentDescType -> Response
okayResponse b c = Response { version = "HTTP/1.1",
                            statuscode = 200,
                            contentDescription = c,
                            body = b }

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
  [(n,[])] -> Just n
  _ -> Nothing

type RequestHandler = Request -> IO Response

fooHandler :: RequestHandler
fooHandler req = return $ Response "HTTP/1.1" 200
                 TEXT_PLAIN "You are looking at /foo/"

fooCatHandler :: RequestHandler
fooCatHandler req = return $ Response "HTTP/1.1" 200
                    TEXT_PLAIN "You are looking at /foo/cat/"

echoHandler :: RequestHandler
echoHandler req = return $ Response "HTTP/1.1" 200 TEXT_PLAIN (show req)

fourOhFourHandler :: RequestHandler
fourOhFourHandler req = return $ Response "HTTP/1.1" 404
                        TEXT_PLAIN "Oh man! 404..."

fileHandler :: String -> RequestHandler
fileHandler s req = do
  contents <- readFile s
  return $ Response "HTTP/1.1" 200 (inferContentDescType s) contents

resourceHandler :: String -> RequestHandler
resourceHandler s req = fileHandler ("res/" ++ s) req

testHandler :: RequestHandler
testHandler = fileHandler "res/test.html"

indexHandler :: RequestHandler
indexHandler = fileHandler "res/index.html"

route1Free :: Route RequestHandler
route1Free =
  choice [ do matchNone
              return $ indexHandler
         , do match "echo"
              matchNone
              return $ echoHandler
         , do match "resource"
              resourceLocation <- capture $ return . id
              return $ resourceHandler resourceLocation
         ]

application :: Request -> IO Response
application req = do
  let ProcessedPath p = path(req)
  let h = runRoute route1Free p
  response <- case h of
               Nothing -> fourOhFourHandler req
               Just handler -> handler req
  return response
