module Data.Queryable
       (QueryableHandle (QueryableHandle,
                         queryableStdIn,
                         queryableStdOut,
                         queryableProcess,
                         queryableKills,
                         expirationDate,
                         queryableState,
                         unprocessedLines),
        QueryableState (RUNNING, IDLE),
        maybeQueryableHandle,
        setQueryableState,
        appendQueryableLine,
        clearQueryableLines,
        checkOnQueryableByUUID,
        createOrFindQueryable) where

import Control.Monad.Loops (iterateWhile)
import Control.Concurrent (threadDelay, forkIO)
import Data.Char (isSpace)
import Data.List (stripPrefix)
import Data.List.Split (splitOn)
import Control.Monad (when)
import System.Process (createProcess, proc, StdStream(CreatePipe), std_out, std_in,
                       ProcessHandle)
import System.IO (Handle, hPutStrLn, hFlush, hClose, hIsEOF, hGetLine)
import qualified Data.Map as Map
import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar, modifyTVar)
import System.Process (waitForProcess)
import Data.Time (UTCTime, getCurrentTime)
import Data.Maybe (fromJust, catMaybes, listToMaybe)

import Aux (nMinutesFromNow)


data QueryableState = RUNNING | IDLE
data QueryableHandle = QueryableHandle { queryableStdIn :: Handle,
                                         queryableStdOut :: Handle,
                                         queryableProcess :: ProcessHandle,
                                         expirationDate :: UTCTime,
                                         queryableState :: QueryableState,
                                         queryableKills :: QueryableHandle -> IO (),
                                         unprocessedLines :: [String] }

maybeQueryableHandle :: Maybe Handle -> Maybe Handle -> Maybe ProcessHandle ->
                        Maybe UTCTime -> Maybe QueryableState ->
                        Maybe (QueryableHandle -> IO ()) -> Maybe [String] ->
                        Maybe QueryableHandle
maybeQueryableHandle (Just hStdIn) (Just hStdOut) (Just hProcess)
  (Just t) (Just s) (Just kh) (Just ls) =
  Just $ QueryableHandle hStdIn hStdOut hProcess t s kh ls
maybeQueryableHandle _ _ _ _ _ _ _ = Nothing

appendQueryableLine :: String -> QueryableHandle -> QueryableHandle
appendQueryableLine l (QueryableHandle hin hout hprocess expiry state killer ls) =
  QueryableHandle hin hout hprocess expiry state killer (ls ++ [l])

clearQueryableLines :: QueryableHandle -> QueryableHandle
clearQueryableLines (QueryableHandle hin hout hprocess expiry state killer  _) =
  QueryableHandle hin hout hprocess expiry state killer []

setQueryableState :: QueryableState -> QueryableHandle -> QueryableHandle
setQueryableState newState (QueryableHandle hin hout hprocess expiry _ killer ls) =
  QueryableHandle hin hout hprocess expiry newState killer ls

checkOnQueryableByUUID :: TVar (Map.Map String QueryableHandle) -> String -> IO Bool
checkOnQueryableByUUID stmHandles uuidString = do
  now <- getCurrentTime

  -- read handles, if now > expirationDate of the handle in the map, then
  -- remove it and prepare to kill
  (shouldKill, mh) <- atomically $ do
    handlesMap <- readTVar stmHandles
    let mHandle = Map.lookup uuidString handlesMap
    case mHandle of
     Just h -> do
       when (now > expirationDate h) $
         writeTVar stmHandles $ Map.delete uuidString handlesMap
       return (now > expirationDate h, mHandle)
     Nothing -> return (False, Nothing)

  when shouldKill $
    queryableKills (fromJust mh) $ fromJust mh

  -- if there is nothing in the map, no need to keep checking
  case mh of
   Nothing -> return False
   _ -> return $ not shouldKill

createQueryable :: String -> [String] ->
                   IO (Maybe Handle, Maybe Handle, Maybe ProcessHandle)
createQueryable c params = do
  (stdinHandle, stdoutHandle, _, processHandle) <-
    createProcess (proc c params){ std_in = CreatePipe,
                                   std_out = CreatePipe}
  return (stdinHandle, stdoutHandle, Just processHandle)

createOrFindQueryable :: String -> [String] -> String ->
                         (QueryableHandle -> IO ()) -> (QueryableHandle -> IO ()) ->
                         TVar (Map.Map String QueryableHandle) ->
                         IO QueryableHandle
-- this is all kinds of buggy, doesn't close handles if some are just and some are not
-- also gross that it opens a process before entering STM
createOrFindQueryable c params uuidString nkiller afterCreation handles = do
  expiry <- nMinutesFromNow 5
  (pStdinHandle, pStdoutHandle, pProcessHandle) <- createQueryable c params
  (h, wasInMap) <- atomically $ do
    unwrappedHandles <- readTVar handles
    let isAlreadyInMap = Map.member uuidString unwrappedHandles
    let newUnwrappedHandles = case isAlreadyInMap of
          -- No engine in the list of running engines, spin up a new one
          False -> do
            let newHandle = maybeQueryableHandle pStdinHandle
                            pStdoutHandle
                            pProcessHandle
                            (Just expiry)
                            (Just IDLE)
                            (Just nkiller)
                            (Just [])
            (Map.insert uuidString (fromJust newHandle) unwrappedHandles)

          -- Found an engine, update the expiration date and return it
          True -> do
            let QueryableHandle stdinHandle stdoutHandle processHandle _ state killer ls =
                  unwrappedHandles Map.! uuidString
            let updatedHandle = QueryableHandle stdinHandle
                                stdoutHandle
                                processHandle
                                expiry
                                state
                                killer
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
         checkOnQueryableByUUID handles uuidString
       return ()

     -- spawn IO monitor for async reads from engine stdout
     let recordLines stmH hRead = do
           atEOF <- hIsEOF hRead
           if atEOF
             then return ()
             else do
               l <- hGetLine hRead
               atomically $ modifyTVar stmH (Map.adjust (appendQueryableLine l) uuidString)
               recordLines stmH hRead
       in forkIO $ recordLines handles (queryableStdOut h)


     -- prep the queryable
     afterCreation h
   True -> do
     -- kill the spawned process
     let toKill = maybeQueryableHandle pStdinHandle pStdoutHandle
                  pProcessHandle (Just expiry) (Just IDLE) (Just nkiller) (Just [])
     case toKill of
      Nothing -> return ()

      Just engine -> do
        let killTechnique = fromJust $ fmap queryableKills toKill
        killTechnique engine

  return h
