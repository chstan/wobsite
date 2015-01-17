{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Handlers
       (RequestHandler,
        projectIndexHandler,
        fourOhFourHandler,
        talksAndPapersHandler,
        resourceHandler,
        tmpResourceHandler,
        indexHandler,
        resumeHandler,
        echoHandler,
        chessHandler,
        computerChessHandler,
        chessResultHandler,
        catHandler,
        contactHandler,
        booksHandler,
        blogIndexHandler,
        blogEntryHandler,
        staticPageHandler,
        gymDataHandler,
        robotsHandler) where

import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format
import System.IO (hFlush)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Control.Concurrent.STM (atomically, modifyTVar, readTVar)
import Data.UUID.V4 (nextRandom)
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Text.Lazy.Encoding
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Aeson          (FromJSON, eitherDecode, encode)
import Control.Applicative ((<$>))

import Data.TCache.Memoization (cachedByKey)

import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html.Renderer.Utf8 as HR

import ResponseRequest
import Data.Config (engineHandles, serverEnv, engineCommand, engineRecordPath)
import Views.StaticViews
import Views.BlogEntry
import Views.Chess

import Data.Gym
import Data.Chess
import Data.BlogEntry (lookupBlogEntry, markdown_location)

import Aux (replaceChar)

type RequestHandler = Request -> IO Response

cachedReadFile :: Bool -> String -> IO BSL8.ByteString
cachedReadFile inProd s = case inProd of
                           True -> cachedByKey s 0 $ BSL8.readFile s
                           False -> BSL8.readFile s


echoHandler :: RequestHandler
echoHandler req = return $ Response "HTTP/1.1" 200 UNZIP PLAIN Dynamic (BSL8.pack $ show req)

jsonHandler :: String -> RequestHandler
jsonHandler s _ = return $ Response "HTTP/1.1" 200 UNZIP JSON Dynamic (BSL8.pack $ s)

fourOhFourHandler :: RequestHandler
fourOhFourHandler _ = return $ Response "HTTP/1.1" 404 UNZIP
                      PLAIN Dynamic (BSL8.pack $ "Oh man! 404...")

fileHandler :: String -> RequestHandler
fileHandler s r = do
  contents <- cachedReadFile (isProduction r) s
  return $ Response "HTTP/1.1" 200 UNZIP (inferContentDescType s)
    (Cacheable []) contents

tmpResourceHandler :: RequestHandler
tmpResourceHandler req =
  resourceHandler resource req
  where resource = Data.List.intercalate "/" (Prelude.tail fragments)
        ProcessedPath fragments = path(req)

resourceHandler :: String -> RequestHandler
resourceHandler s = fileHandler ("res/" ++ s)

indexHandler :: RequestHandler
indexHandler _ = return $ Response "HTTP/1.1" 200 UNZIP HTML (Cacheable []) $
                 HR.renderHtml $ indexView

contactHandler :: RequestHandler
contactHandler _ = return $ Response "HTTP/1.1" 200 UNZIP HTML (Cacheable []) $
                   HR.renderHtml $ contactView

chessResultHandler :: String -> String -> RequestHandler
chessResultHandler uuid result req = do
  mHandle <- atomically $ do
    handlesMap <- readTVar handles
    let mh = Map.lookup uuid handlesMap
    case mh of
     Nothing -> return Nothing
     Just _ -> do
       modifyTVar handles (Map.delete uuid)
       return mh

  case mHandle of
   -- no game to finish by this uuid
   Nothing -> jsonHandler (chessResultJSONView "NO_GAME") req
   Just h -> do
     -- kill the engine
     killEngine h
     -- update the results
     let rp = (engineRecordPath $ serverEnv $ serverConfig req)

     -- strict byte string read file to avoid file lock error
     recordString <- fmap BS8.unpack $ BS8.readFile rp
     let r = fromMaybe (GameRecord 0 0 0) $ recordFromString $ trim recordString
     writeFile rp $ (show $ updateRecord result r)

     -- let the client know that all went well
     jsonHandler (chessResultJSONView "FINISHED_GAME") req

  where handles = engineHandles $ serverConfig req

computerChessHandler :: RequestHandler
computerChessHandler req
  | Prelude.length pathElems < 3 = jsonHandler (chessJSONView "" []) req
  | otherwise = do
      engineHandle <- createOrFindEngine (engineCommand $ serverEnv $ serverConfig req)
                      uuidString (engineHandles $ serverConfig req)
      case engineState engineHandle of
       --engine is not currently searching for a move
       --start it searching and change state to running
       --return no info for simplicity (will take some time
       --for the engine to search)
       IDLE -> do
         atomically $ modifyTVar (engineHandles $ serverConfig req)
           (Map.adjust (setEngineState RUNNING) uuidString)
         BSL8.hPutStrLn (engineStdIn engineHandle) (BSL8.pack $ "position fen " ++ fenString)
         BSL8.hPutStrLn (engineStdIn engineHandle) (BSL8.pack $ "go wtime 120000 btime 120000 winc 0 binc 0")
         hFlush (engineStdIn engineHandle)
         jsonHandler (chessJSONView "" []) req

       --engine is searching, don't write to the engine, just read from output
       --if best move is encountered, change state to idle and return the move
       RUNNING -> do
         let engineLines = unprocessedLines engineHandle
         case (bestMoveFromEngineOutput engineLines) of
          -- found a move, update the engine state and return it
          Just move -> do
            atomically $ do
              modifyTVar (engineHandles $ serverConfig req)
                (Map.adjust clearEngineLines uuidString)
              modifyTVar (engineHandles $ serverConfig req)
                (Map.adjust (setEngineState IDLE) uuidString)
            jsonHandler (chessJSONView move engineLines) req

          -- no move, ah well
          Nothing -> jsonHandler (chessJSONView "" []) req

  where ProcessedPath pathElems = path(req)
        fenString = replaceChar '~' '/' (replaceChar '_' ' ' rawFenString)
        (uuidString, rawFenString) = (pathElems !! 1, pathElems !! 2)

chessHandler :: RequestHandler
chessHandler req = do
  uuid <- nextRandom
  -- Don't cache this file! It is updated throughout execution
  recordString <- Prelude.readFile (engineRecordPath $ serverEnv $ serverConfig req)
  return $ Response "HTTP/1.1" 200 UNZIP HTML Dynamic $
    HR.renderHtml $ chessView uuid $ recordFromString $ trim recordString

catHandler :: RequestHandler
catHandler _ = return $ Response "HTTP/1.1" 200 UNZIP HTML (Cacheable []) $
               HR.renderHtml $ catView

robotsHandler :: RequestHandler
robotsHandler = resourceHandler "robots.txt"

genIndexHandler :: (FromJSON a) => String -> ([a] -> Html) -> RequestHandler
genIndexHandler indexFile view req = do
  d <- eitherDecode <$> cachedReadFile (isProduction req) indexFile
  case d of
   Left _ -> fourOhFourHandler req
   Right entries -> return $ Response "HTTP/1.1" 200 UNZIP HTML (Cacheable []) $
                    HR.renderHtml $ view entries

genStaticPageHandler :: String -> String -> RequestHandler
genStaticPageHandler indexFile name req = do
  d <- eitherDecode <$> cachedReadFile (isProduction req) indexFile
  case (lookupBlogEntry name d) of
   Nothing -> fourOhFourHandler req
   Just l -> do
     c <- fmap decodeUtf8 $ cachedReadFile (isProduction req)
          ("res/" ++ (T.unpack $ markdown_location l))
     return $ Response "HTTP/1.1" 200 UNZIP HTML (Cacheable []) $
       HR.renderHtml $ blogEntryView l c

talksAndPapersHandler :: RequestHandler
talksAndPapersHandler = genIndexHandler "res/conferences.json" talksAndPapersView

booksHandler :: RequestHandler
booksHandler = genIndexHandler "res/books.json" booksView

blogIndexHandler :: RequestHandler
blogIndexHandler = genIndexHandler "res/blog_entries.json" blogIndexView

projectIndexHandler :: RequestHandler
projectIndexHandler = genIndexHandler "res/project_descriptions.json" projectIndexView

staticPageHandler :: String -> RequestHandler
staticPageHandler = genStaticPageHandler "res/pages.json"

blogEntryHandler :: String -> RequestHandler
blogEntryHandler = genStaticPageHandler "res/blog_entries.json"

resumeHandler :: RequestHandler
resumeHandler = genIndexHandler "res/conferences.json" resumeView

gregorianTimeString :: IO String
gregorianTimeString = do
  (y, m, d) <- gregorianTime
  return $ intercalate "-" $ fmap show [m, d] ++ [(show y)]

gregorianTime :: IO (Integer, Int, Int)
gregorianTime = do
  now <- getCurrentTime
  zone <- getTimeZone now
  return $ toGregorian $ localDay $ zonedTimeToLocalTime $ utcToZonedTime zone now

bodyWeightHandler :: Float -> RequestHandler
bodyWeightHandler w req = do
  let wf = "res/weight.json"
  !a <- eitherDecode <$> BSL8.readFile wf
  t <- gregorianTimeString
  let updated = case (a :: Either String WeightMap) of
        Left _ -> Map.singleton t $ Weight w
        Right m -> Map.insert t (Weight w) m
  BSL8.writeFile wf $ encode updated
  jsonHandler "{}" req

exerciseHandler :: String -> Maybe Float -> Int -> Int -> RequestHandler
exerciseHandler n mw r s req = do
  let wf = "res/gym_data.json"
  !a <- eitherDecode <$> BSL8.readFile wf
  t <- gregorianTimeString
  let updated = case (a :: Either String ExerciseMap) of
        Left _ -> Map.singleton t $ Map.singleton n (Exercise mw r s)
        Right m -> case Map.lookup t m of
          Nothing -> Map.insert t (Map.singleton n (Exercise mw r s)) m
          Just inm -> Map.insert t (Map.insert n (Exercise mw r s) inm) m
  BSL8.writeFile wf $ encode updated
  jsonHandler "{}" req

gymDataHandler :: String -> Maybe Float -> Maybe Int -> Maybe Int -> RequestHandler
gymDataHandler "Weight" (Just w) Nothing Nothing = bodyWeightHandler w
gymDataHandler n mw (Just r) (Just s) = exerciseHandler (replaceChar '_' ' ' n) mw r s
gymDataHandler _ _ _ _ = fourOhFourHandler
