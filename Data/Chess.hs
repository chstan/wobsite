module Data.Chess
       (killEngine,
        bestMoveFromEngineOutput,
        chessAfterCreation,

        GameRecord (GameRecord),
        updateRecord,
        recordFromString,

        trim
       ) where

import Data.Char (isSpace)
import Data.List (stripPrefix)
import Data.List.Split (splitOn)
import System.IO (hPutStrLn, hFlush, hClose, hIsEOF, hGetLine)
import System.Process (waitForProcess)
import Data.Time (UTCTime, getCurrentTime)
import Data.Maybe (fromJust, catMaybes, listToMaybe)

import Aux (nMinutesFromNow)
import Data.Queryable

data GameRecord = GameRecord Int Int Int
instance Show GameRecord where
  show (GameRecord w l t) =
    (show w) ++ "-" ++ (show l) ++ "-" ++ (show t)

updateRecord :: String -> GameRecord -> GameRecord
updateRecord "win" (GameRecord w l t) = GameRecord (w+1) l t
updateRecord "loss" (GameRecord w l t) = GameRecord w (l+1) t
updateRecord "tie" (GameRecord w l t) = GameRecord w l (t+1)
updateRecord _ g = g

chessAfterCreation :: QueryableHandle -> IO ()
chessAfterCreation h = do
  hPutStrLn (queryableStdIn h) "uci"
  hPutStrLn (queryableStdIn h) "isready"
  hFlush (queryableStdIn h)

recordFromString :: String -> Maybe GameRecord
recordFromString l = recordFromSplitLine ls
  where ls = fmap read $ splitOn "-" l
        recordFromSplitLine (win:loss:tie:[]) = Just $ GameRecord win loss tie
        recordFromSplitLine _ = Nothing

killEngine :: QueryableHandle -> IO ()
killEngine h = do
  -- kill according to UCI specification
  hPutStrLn (queryableStdIn h) "stop"
  hPutStrLn (queryableStdIn h) "quit"
  hFlush (queryableStdIn h)
  hClose (queryableStdIn h)
  hClose (queryableStdOut h)
  waitForProcess (queryableProcess h)
  return ()

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

bestMoveFromEngineOutput :: [String] -> Maybe String
bestMoveFromEngineOutput ls =
  fmap trim  $ listToMaybe $ catMaybes $ fmap (stripPrefix "bestmove") ls
