module Handlers
       (RequestHandler,
        fourOhFourHandler,
        resourceHandler,
        indexHandler,
        echoHandler) where

import ResponseRequest
import Aux                 (inferContentDescType)

type RequestHandler = Request -> IO Response

echoHandler :: RequestHandler
echoHandler req = return $ Response "HTTP/1.1" 200 TEXT_PLAIN (show req)

fourOhFourHandler :: RequestHandler
fourOhFourHandler _ = return $ Response "HTTP/1.1" 404
                        TEXT_PLAIN "Oh man! 404..."

fileHandler :: String -> RequestHandler
fileHandler s _ = do
  contents <- readFile s
  return $ Response "HTTP/1.1" 200 (inferContentDescType s) contents

resourceHandler :: String -> RequestHandler
resourceHandler s req = fileHandler ("res/" ++ s) req

indexHandler :: RequestHandler
indexHandler = fileHandler "res/index.html"
