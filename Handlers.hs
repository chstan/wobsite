{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Handlers
       (RequestHandler,
        projectIndexHandler,
        fourOhFourHandler,
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
        robotsHandler) where

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
import Data.Aeson          (FromJSON, eitherDecode)
import Control.Applicative ((<$>))

import Data.TCache.Memoization (cachedByKey)

import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html.Renderer.Utf8 as HR

import ResponseRequest
import Data.Config (engineHandles, serverEnv, engineCommand, engineRecordPath)
import Views.StaticViews
import Views.BlogEntry
import Views.Chess

import Data.Chess
import Data.BlogEntry (lookupBlogEntry, markdown_location)

type RequestHandler = Request -> IO Response

cachedReadFile :: String -> IO BSL8.ByteString
cachedReadFile s = cachedByKey s 0 $ BSL8.readFile s

echoHandler :: RequestHandler
echoHandler req = return $ Response "HTTP/1.1" 200 UNZIP PLAIN Dynamic (BSL8.pack $ show req)

jsonHandler :: String -> RequestHandler
jsonHandler s _ = return $ Response "HTTP/1.1" 200 UNZIP JSON Dynamic (BSL8.pack $ s)

fourOhFourHandler :: RequestHandler
fourOhFourHandler _ = return $ Response "HTTP/1.1" 404 UNZIP
                      PLAIN Dynamic (BSL8.pack $ "Oh man! 404...")

fileHandler :: String -> RequestHandler
fileHandler s _ = do
  contents <- cachedReadFile s
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

resumeHandler :: RequestHandler
resumeHandler _ = return $ Response "HTTP/1.1" 200 UNZIP HTML (Cacheable []) $
                HR.renderHtml $ resumeView

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
        replaceFENSpecialChars '_' = ' '
        replaceFENSpecialChars '~' = '/'
        replaceFENSpecialChars c = c

        fenString = Prelude.map replaceFENSpecialChars rawFenString
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
  d <- eitherDecode <$> cachedReadFile indexFile
  case d of
   Left _ -> fourOhFourHandler req
   Right entries -> return $ Response "HTTP/1.1" 200 UNZIP HTML (Cacheable []) $
                    HR.renderHtml $ view entries

genStaticPageHandler :: String -> String -> RequestHandler
genStaticPageHandler indexFile name req = do
  d <- eitherDecode <$> cachedReadFile indexFile
  case (lookupBlogEntry name d) of
   Nothing -> fourOhFourHandler req
   Just l -> do
     c <- fmap decodeUtf8 $ cachedReadFile ("res/" ++ (T.unpack $ markdown_location l))
     return $ Response "HTTP/1.1" 200 UNZIP HTML (Cacheable []) $
       HR.renderHtml $ blogEntryView l c

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
