module Data.Dominion
       (killDominion) where

import Data.Queryable (QueryableHandle(..))
import System.IO (hPutStrLn, hFlush, hClose)
import System.Process (waitForProcess, terminateProcess)

killDominion :: QueryableHandle -> IO ()
killDominion h = do
  hFlush (queryableStdIn h)
  hClose (queryableStdIn h)
  hClose (queryableStdOut h)
  terminateProcess (queryableProcess h)
  waitForProcess (queryableProcess h)
  return ()
