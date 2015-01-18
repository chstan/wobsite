module App
       (application) where

import ResponseRequest
import Handlers
import Routes

import Aux(readPrefix)
import Data.List (stripPrefix)
import Safe (readMay)

-- throw a random request at the router

applicationRoutes :: Route RequestHandler
applicationRoutes =
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

         , do match "chess"
              matchNone
              return chessHandler

         , do match "chess"
              match "result"
              uuid <- capture $ return . id
              result <- capture $ return . id
              return $ chessResultHandler uuid result

         , do match "chess"
              return computerChessHandler

         , do match "resource"
              return $ tmpResourceHandler -- please forgive me

         , do match "resource"
              resourceLocation <- capture $ return . id
              return $ resourceHandler resourceLocation

         , do match "contact"
              return contactHandler

         , do match "projects"
              matchNone
              return projectIndexHandler

         , do match "conferences"
              matchNone
              return talksAndPapersHandler

         , do match "books"
              matchNone
              return booksHandler

         , do match "writing"
              matchNone
              return blogIndexHandler

         , do match "writing"
              entryName <- capture $ return . id
              matchNone
              return $ blogEntryHandler entryName

         , do match "exercise"
              matchNone
              return $ exerciseFormHandler
         , do match "exercise"
              name <- capture $ return . id
              weight <- fmap readMay $ capture $ stripPrefix "weight"
              repetitions <- fmap readMay $ capture $ stripPrefix "repetitions"
              sets <- fmap readMay $ capture $ stripPrefix "sets"
              return $ gymDataHandler name weight repetitions sets

         , do match "static"
              entryName <- capture $ return . id
              matchNone
              return $ staticPageHandler entryName

         , do match "robots.txt"
              return robotsHandler
         ]

application :: Request -> IO Response
application req = do
  let ProcessedPath p = path(req)
  let h = runRoute applicationRoutes p
  response <- case h of
               Nothing -> fourOhFourHandler req
               Just handler -> handler req
  return response
