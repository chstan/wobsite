module Data.Scheme
       (killScheme) where

import Data.Queryable (QueryableHandle(..))
import System.IO (hPutStrLn, hFlush, hClose)
import System.Process (waitForProcess)

killScheme :: QueryableHandle -> IO ()
killScheme h = do
  hFlush (queryableStdIn h)
  hClose (queryableStdIn h)
  hClose (queryableStdOut h)
  waitForProcess (queryableProcess h)
  return ()
