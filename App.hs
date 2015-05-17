module App
       (application) where

import ResponseRequest
import Handlers
import Routes

import qualified Data.Map as Map
import Aux(readPrefix)
import Data.List (stripPrefix, intercalate)
import Safe (readMay)

-- throw a random request at the router

collectMap :: (String -> Maybe a) -> Route (Map.Map String a)
collectMap g = f Map.empty
  where f m = choice [ do k <- capture $ return . id
                          v <- capture g
                          f $ Map.insert k v m
                      , do matchNone
                           return m
                      , zero
                      ]

collectStrings :: Route [String]
collectStrings = f []
  where f xs = choice [ do s <- capture $ return . id
                           f $ xs ++ [s]
                      , return xs
                      ]

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

         , do match "getUUID"
              matchNone
              return genUUIDHandler

         , do match "eval"
              matchNone
              return schemeEvalHandler

         , do match "fuzzy_talk"
              matchNone
              return fuzzyTalkHandler

         , do match "plotly"
              matchNone
              return plotlyHandler

         , do match "chess"
              match "result"
              uuid <- capture $ return . id
              result <- capture $ return . id
              return $ chessResultHandler uuid result

         , do match "scheme"
              matchNone
              return schemeHandler

         , do match "chess"
              return computerChessHandler

         , do match "resource"
              ss <- collectStrings
              return $ resourceHandler $ intercalate "/" ss

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
              choice [ do matchNone
                          return $ exerciseFormHandler
                     , do match "graph"
                          matchNone
                          return exerciseGraphHandler
                     , do match "data"
                          dataset <- capture $ return . id
                          return $ exerciseDataRetrievalHandler dataset
                     , do name <- capture $ return . id
                          weight <- fmap readMay $ capture $ stripPrefix "weight"
                          repetitions <- fmap readMay $ capture $ stripPrefix "repetitions"
                          sets <- fmap readMay $ capture $ stripPrefix "sets"
                          return $ gymDataHandler name weight repetitions sets
                     ]

         , do match "static"
              entryName <- capture $ return . id
              matchNone
              return $ staticPageHandler entryName

         , do match "robots.txt"
              return robotsHandler
         ]

application :: Request -> IO Response
application req = do
  putStrLn $ show req
  let ProcessedPath p = path(req)
  let h = runRoute applicationRoutes p
  response <- case h of
               Nothing -> fourOhFourHandler req
               Just handler -> handler req
  return response
