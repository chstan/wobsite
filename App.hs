module App
       (application) where

import ResponseRequest
import Handlers
import Routes

-- throw a random request at the router

route1Free :: Route RequestHandler
route1Free =
  choice [ do matchNone
              return indexHandler
         , do match "resume"
              return resumeHandler
         , do match "echo"
              matchNone
              return echoHandler
         , do match "super"
              match "secret"
              match "cat"
              return catHandler
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
