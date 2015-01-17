{-# LANGUAGE ViewPatterns #-}

module Aux
       (pairFn,
        tailSafe,
        oneAndMatches,
        dummyParam,
        nMinutesFromNow,
        readPrefix,
        replaceChar
        ) where


--myFunc (stripPrefix "toaster" -> Just restOfString) = -- do something special
--myFunc string = -- do the default case here

import Data.List
import Data.Time.Clock
import Data.Time.Lens
import Safe (readMay)

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

readPrefix :: (Read a) => String -> String -> Maybe a
readPrefix p (stripPrefix p -> Just r) = readMay r
readPrefix _ _ = Nothing

replaceChar :: Char -> Char -> String -> String
replaceChar o n s = fmap repl s
  where repl c = case c == o of
          True -> n
          False -> c
