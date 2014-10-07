module Handlers
       (RequestHandler,
        fourOhFourHandler,
        resourceHandler,
        indexHandler,
        resumeHandler,
        echoHandler) where

import Data.ByteString.Lazy.Char8 as BSL8
import ResponseRequest
import Aux                 (inferContentDescType)

type RequestHandler = Request -> IO Response

echoHandler :: RequestHandler
echoHandler req = return $ Response "HTTP/1.1" 200 TEXT_PLAIN (pack $ show req)

fourOhFourHandler :: RequestHandler
fourOhFourHandler _ = return $ Response "HTTP/1.1" 404
                      TEXT_PLAIN (pack $ "Oh man! 404...")

fileHandler :: String -> RequestHandler
fileHandler s _ = do
  contents <- BSL8.readFile s
  return $ Response "HTTP/1.1" 200 (inferContentDescType s) contents

resourceHandler :: String -> RequestHandler
resourceHandler s req = fileHandler ("res/" ++ s) req

indexHandler :: RequestHandler
indexHandler = resourceHandler "index.html"

resumeHandler :: RequestHandler
resumeHandler = resourceHandler "resume.html"
