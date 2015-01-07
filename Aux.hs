module Aux
       (pairFn,
        tailSafe,
        oneAndMatches,
        dummyParam,
        nMinutesFromNow,
        ) where

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
