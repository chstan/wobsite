module Aux
       (pairFn,
        inferContentDescType,
        tailSafe,
        oneAndMatches
        ) where

import System.FilePath (takeExtension)

import ResponseRequest (ContentDescType (..))


pairFn :: (a -> b) -> (a -> c) -> a -> (b, c)
pairFn fn1 fn2 el =
  (fn1 el, fn2 el)

inferContentDescType :: String -> ContentDescType
inferContentDescType loc = case takeExtension loc of
  ".css" -> TEXT_CSS
  ".html" -> TEXT_HTML
  _ -> TEXT_PLAIN

tailSafe :: [a] -> [a]
tailSafe [] = []
tailSafe l = tail l

oneAndMatches :: (Eq a) => a -> [a] -> Bool
oneAndMatches p [] = False
oneAndMatches p (x:xs)
  | p == x = True
  | otherwise = False
