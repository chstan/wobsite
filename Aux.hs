module Aux
       (pairFn,
        tailSafe,
        oneAndMatches,
        dummyParam,
        nMinutesFromNow,
        ) where

import qualified Control.Exception as Exc
import System.IO
import Data.Time.Clock
import Data.Time.Lens

pairFn :: (a -> b) -> (a -> c) -> a -> (b, c)
pairFn fn1 fn2 el =
  (fn1 el, fn2 el)

tailSafe :: [a] -> [a]
tailSafe [] = []
tailSafe l = tail l

oneAndMatches :: (Eq a) => a -> [a] -> Bool
oneAndMatches _ [] = False
oneAndMatches p (x:_)
  | p == x = True
  | otherwise = False

dummyParam :: a -> b -> a
dummyParam f _ = f

nMinutesFromNow :: Int -> IO UTCTime
nMinutesFromNow n = do
  now <- getCurrentTime
  return $ modL minutes (+n) now

_hReadLinesNew :: [String] -> Handle -> IO [String]
_hReadLinesNew ls h = do
  putStrLn "Watching for exception."
  ml <- Exc.catch (fmap Just $ hGetLine h) handler
  putStrLn $ show ml
  case ml of
   Just l -> do
     _hReadLinesNew (l:ls) h
   Nothing -> return $ reverse ls
  where
    handler :: Exc.ErrorCall -> IO (Maybe String)
    handler _ = return Nothing

_hReadLines :: [String] -> Handle -> IO [String]
_hReadLines ls h = do
  atEOF <- hIsEOF h
  if atEOF
    then do
      return $ reverse ls
    else do
      l <- hGetLine h
      putStrLn l
      _hReadLines (l:ls) h

hReadLines :: Handle -> IO [String]
hReadLines h = do
  _hReadLinesNew [] h
