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
        bestMoveFromEngineOutput
       ) where

import Data.Char (isSpace)
import Data.List (stripPrefix)
import Control.Monad (when)
import System.IO (Handle, hPutStrLn)
import qualified Data.Map as Map
import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import System.Process (ProcessHandle)
import Data.Time (UTCTime, getCurrentTime)
import Data.Maybe (fromJust, catMaybes, listToMaybe)

data ChessEngineState = RUNNING | IDLE
data ChessEngineHandle = ChessEngineHandle { engineStdIn :: Handle,
                                             engineStdOut :: Handle,
                                             engineProcess :: ProcessHandle,
                                             expirationDate :: UTCTime,
                                             engineState :: ChessEngineState,
                                             unprocessedLines :: [String] }

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
     Nothing -> return (False, mHandle)

  when shouldKill $
    killEngine $ fromJust mh
  return $ not shouldKill

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

bestMoveFromEngineOutput :: [String] -> Maybe String
bestMoveFromEngineOutput ls =
  fmap trim  $ listToMaybe $ catMaybes $ fmap (stripPrefix "bestmove") ls
