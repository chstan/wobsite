module Data.Chess
       (ChessEngineHandle (ChessEngineHandle,
                           engineStdIn,
                           engineStdOut,
                           engineProcess,
                           expirationDate,
                           engineState,
                           unprocessedLines),
        ChessEngineState (RUNNING, IDLE),
        maybeChessEngineHandle,
        setEngineState,
        appendEngineLine,
        clearEngineLines,
        killEngine,
        checkOnEngineByUUID,
        bestMoveFromEngineOutput,
        createOrFindEngine,

        GameRecord (GameRecord),
        updateRecord,
        recordFromString,

        trim
       ) where

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

data ChessEngineState = RUNNING | IDLE
data ChessEngineHandle = ChessEngineHandle { engineStdIn :: Handle,
                                             engineStdOut :: Handle,
                                             engineProcess :: ProcessHandle,
                                             expirationDate :: UTCTime,
                                             engineState :: ChessEngineState,
                                             unprocessedLines :: [String] }

data GameRecord = GameRecord Int Int Int
instance Show GameRecord where
  show (GameRecord w l t) =
    (show w) ++ "-" ++ (show l) ++ "-" ++ (show t)

updateRecord :: String -> GameRecord -> GameRecord
updateRecord "win" (GameRecord w l t) = GameRecord (w+1) l t
updateRecord "loss" (GameRecord w l t) = GameRecord w (l+1) t
updateRecord "tie" (GameRecord w l t) = GameRecord w l (t+1)
updateRecord _ g = g

recordFromString :: String -> Maybe GameRecord
recordFromString l = recordFromSplitLine ls
  where ls = fmap read $ splitOn "-" l
        recordFromSplitLine (win:loss:tie:[]) = Just $ GameRecord win loss tie
        recordFromSplitLine _ = Nothing


maybeChessEngineHandle :: Maybe Handle -> Maybe Handle -> Maybe ProcessHandle ->
                          Maybe UTCTime -> Maybe ChessEngineState -> Maybe [String] ->
                          Maybe ChessEngineHandle
maybeChessEngineHandle (Just hStdIn) (Just hStdOut) (Just hProcess)
  (Just t) (Just s) (Just ls) =
  Just $ ChessEngineHandle hStdIn hStdOut hProcess t s ls
maybeChessEngineHandle _ _ _ _ _ _ = Nothing

appendEngineLine :: String -> ChessEngineHandle -> ChessEngineHandle
appendEngineLine l (ChessEngineHandle hin hout hprocess expiry state ls) =
  ChessEngineHandle hin hout hprocess expiry state (ls ++ [l])

clearEngineLines :: ChessEngineHandle -> ChessEngineHandle
clearEngineLines (ChessEngineHandle hin hout hprocess expiry state _) =
  ChessEngineHandle hin hout hprocess expiry state []

setEngineState :: ChessEngineState -> ChessEngineHandle -> ChessEngineHandle
setEngineState newState (ChessEngineHandle hin hout hprocess expiry _ ls) =
  ChessEngineHandle hin hout hprocess expiry newState ls

killEngine :: ChessEngineHandle -> IO ()
killEngine h = do
  -- kill according to UCI specification
  hPutStrLn (engineStdIn h) "stop"
  hPutStrLn (engineStdIn h) "quit"
  hFlush (engineStdIn h)
  hClose (engineStdIn h)
  hClose (engineStdOut h)
  waitForProcess (engineProcess h)
  return ()

checkOnEngineByUUID :: TVar (Map.Map String ChessEngineHandle) -> String -> IO Bool
checkOnEngineByUUID stmHandles uuidString = do
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
    killEngine $ fromJust mh

  -- if there is nothing in the map, no need to keep checking
  case mh of
   Nothing -> return False
   _ -> return $ not shouldKill

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

bestMoveFromEngineOutput :: [String] -> Maybe String
bestMoveFromEngineOutput ls =
  fmap trim  $ listToMaybe $ catMaybes $ fmap (stripPrefix "bestmove") ls

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
  expiry <- nMinutesFromNow 5
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
