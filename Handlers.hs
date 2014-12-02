{-# LANGUAGE OverloadedStrings #-}

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
        catHandler,
        contactHandler,
        booksHandler,
        blogIndexHandler,
        blogEntryHandler,
        staticPageHandler,
        robotsHandler) where

import Control.Monad.Loops (iterateWhile)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Control.Monad.STM (STM)
import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar, modifyTVar)
import System.IO (Handle, hFlush, hGetLine, hIsEOF)
import System.Process (createProcess, proc, StdStream(CreatePipe), std_out, std_in,
                       ProcessHandle)
import Data.UUID.V4 (nextRandom)
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TLIO
import Data.Text.Lazy.Encoding
import Data.ByteString.Lazy.Char8 as BSL8
import Data.Aeson          (eitherDecode)
import Control.Applicative ((<$>))

import Data.TCache.Memoization (cachedByKey)

import qualified Text.Blaze.Html.Renderer.Utf8 as HR

import ResponseRequest
import Data.Config (engineHandles, serverEnv, engineCommand)
import Aux                 (inferContentDescType, fiveMinutesFromNow, hReadLines)
import Views.StaticViews
import Views.BlogEntry
import Views.Chess

import Data.Chess
import Data.BlogEntry (lookupBlogEntry, markdown_location)

type RequestHandler = Request -> IO Response

cachedReadFile :: String -> IO ByteString
cachedReadFile s = cachedByKey s 0 $ BSL8.readFile s

echoHandler :: RequestHandler
echoHandler req = return $ Response "HTTP/1.1" 200 UNZIP PLAIN (pack $ show req)

jsonHandler :: String -> RequestHandler
jsonHandler s _ = return $ Response "HTTP/1.1" 200 UNZIP JSON (pack $ s)

fourOhFourHandler :: RequestHandler
fourOhFourHandler _ = return $ Response "HTTP/1.1" 404 UNZIP
                      PLAIN (pack $ "Oh man! 404...")

fileHandler :: String -> RequestHandler
fileHandler s _ = do
  contents <- cachedReadFile s
  return $ Response "HTTP/1.1" 200 UNZIP (inferContentDescType s) contents

tmpResourceHandler :: RequestHandler
tmpResourceHandler req =
  resourceHandler resource req
  where resource = Data.List.intercalate "/" (Prelude.tail fragments)
        ProcessedPath fragments = path(req)

resourceHandler :: String -> RequestHandler
resourceHandler s = fileHandler ("res/" ++ s)

indexHandler :: RequestHandler
indexHandler _ = return $ Response "HTTP/1.1" 200 UNZIP HTML $
                 HR.renderHtml $ indexView

contactHandler :: RequestHandler
contactHandler _ = return $ Response "HTTP/1.1" 200 UNZIP HTML $
                   HR.renderHtml $ contactView

resumeHandler :: RequestHandler
resumeHandler _ = return $ Response "HTTP/1.1" 200 UNZIP HTML $
                HR.renderHtml $ resumeView

createChessEngine :: String -> IO (Maybe Handle, Maybe Handle, Maybe ProcessHandle)
createChessEngine c = do
  (stdinHandle, stdoutHandle, _, processHandle) <-
    createProcess (proc c ["--uci", "engine.log"]){ std_in = CreatePipe,
                                                    std_out = CreatePipe}
  return (stdinHandle, stdoutHandle, Just processHandle)

createOrFindEngine :: String -> String -> TVar (Map.Map String ChessEngineHandle) ->
                      IO ChessEngineHandle
-- this is all kinds of buggy, doesn't close handles if some are just and some are not
-- also gross that it opens a process before entering STM
createOrFindEngine c uuidString handles = do
  expiry <- fiveMinutesFromNow
  (pStdinHandle, pStdoutHandle, pProcessHandle) <- createChessEngine c
  (h, wasInMap) <- atomically $ do
    unwrappedHandles <- readTVar handles
    let isAlreadyInMap = Map.member uuidString unwrappedHandles
    let newUnwrappedHandles = case isAlreadyInMap of
          -- No engine in the list of running engines, spin up a new one
          False -> do
            let newHandle = maybeChessEngineHandle pStdinHandle
                            pStdoutHandle
                            pProcessHandle
                            (Just expiry)
                            (Just IDLE)
                            (Just [])
            (Map.insert uuidString (fromJust newHandle) unwrappedHandles)

          -- Found an engine, update the expiration date and return it
          True -> do
            let ChessEngineHandle stdinHandle stdoutHandle processHandle _ state ls =
                  unwrappedHandles Map.! uuidString
            let updatedHandle = ChessEngineHandle stdinHandle
                                stdoutHandle
                                processHandle
                                expiry
                                state
                                ls
            Map.insert uuidString updatedHandle unwrappedHandles
    let foundHandle = newUnwrappedHandles Map.! uuidString
    writeTVar handles newUnwrappedHandles
    return (foundHandle, isAlreadyInMap)
  case wasInMap of
   False -> do
     -- spawn watcher
     forkIO $ do
       -- check until we killed it because no one was using, can't trust the client!
       iterateWhile id $ do
         -- wait five minutes
         threadDelay (5 * 60 * 1000000)
         checkOnEngineByUUID handles uuidString
       return ()

     -- spawn IO monitor for async reads from engine stdout
     let recordLines stmH hRead = do
           atEOF <- hIsEOF hRead
           if atEOF
             then return ()
             else do
               l <- hGetLine hRead
               atomically $ modifyTVar stmH (Map.adjust (appendEngineLine l) uuidString)
               recordLines stmH hRead
       in forkIO $ recordLines handles (engineStdOut h)


     -- prep the engine
     hPutStrLn (engineStdIn h) "uci"
     hPutStrLn (engineStdIn h) "isready"
     hFlush (engineStdIn h)
     return ()
   True -> do
     -- kill the spawned process
     let toKill = maybeChessEngineHandle pStdinHandle pStdoutHandle
                  pProcessHandle (Just expiry) (Just IDLE) (Just [])
     case toKill of
      Nothing -> return ()
      Just engine -> killEngine engine

  return h

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
         hPutStrLn (engineStdIn engineHandle) (pack $ "position fen " ++ fenString)
         hPutStrLn (engineStdIn engineHandle) (pack $ "go wtime 120000 btime 120000 winc 0 binc 0")
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
chessHandler _ = do
  uuid <- nextRandom
  return $ Response "HTTP/1.1" 200 UNZIP HTML $
    HR.renderHtml $ chessView uuid

catHandler :: RequestHandler
catHandler _ = return $ Response "HTTP/1.1" 200 UNZIP HTML $
               HR.renderHtml $ catView

robotsHandler :: RequestHandler
robotsHandler = resourceHandler "robots.txt"

projectIndexHandler :: RequestHandler
projectIndexHandler req = do
  dec <- eitherDecode <$> cachedReadFile "res/project_descriptions.json"
  case dec of
   Left _ -> fourOhFourHandler req -- Meh, could be a better response.
   Right projects -> return $ Response "HTTP/1.1" 200 UNZIP HTML $
                     HR.renderHtml $ projectIndexView projects

booksHandler :: RequestHandler
booksHandler req = do
  dec <- eitherDecode <$> cachedReadFile "res/books.json"
  case dec of
   Left _ -> fourOhFourHandler req
   Right books -> return $ Response "HTTP/1.1" 200 UNZIP HTML $
                  HR.renderHtml $ booksView books

staticPageHandler :: String -> RequestHandler
staticPageHandler entryName req = do
  dec <- eitherDecode <$> cachedReadFile "res/pages.json"
  case (lookupBlogEntry entryName dec) of
    Nothing -> fourOhFourHandler req
    Just listing -> do
      content <- fmap decodeUtf8 $ cachedReadFile ("res/" ++ (T.unpack m_location))
      return $ Response "HTTP/1.1" 200 UNZIP HTML $
        HR.renderHtml $ blogEntryView listing content
       where m_location = markdown_location listing


blogIndexHandler :: RequestHandler
blogIndexHandler req = do
  dec <- eitherDecode <$> cachedReadFile "res/blog_entries.json"
  case dec of
   Left _ -> fourOhFourHandler req
   Right entries -> return $ Response "HTTP/1.1" 200 UNZIP HTML $
                    HR.renderHtml $ blogIndexView entries

blogEntryHandler :: String -> RequestHandler
blogEntryHandler entryName req = do
  dec <- eitherDecode <$> cachedReadFile "res/blog_entries.json"
  case (lookupBlogEntry entryName dec) of
    Nothing -> fourOhFourHandler req
    Just listing -> do
      content <- fmap decodeUtf8 $ cachedReadFile ("res/" ++ (T.unpack m_location))
      return $ Response "HTTP/1.1" 200 UNZIP HTML $
        HR.renderHtml $ blogEntryView listing content
       where m_location = markdown_location listing
