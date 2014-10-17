module Aux
       (pairFn,
        inferContentDescType,
        tailSafe,
        oneAndMatches,
        dummyParam
        ) where

import System.FilePath (takeExtension)

import ResponseRequest (ContentDescType (..))


pairFn :: (a -> b) -> (a -> c) -> a -> (b, c)
pairFn fn1 fn2 el =
  (fn1 el, fn2 el)

inferContentDescType :: String -> ContentDescType
inferContentDescType loc = case takeExtension loc of
  ".css" -> CSS
  ".html" -> HTML
  ".pdf" -> PDF
  ".png" -> PNG
  ".jpg" -> JPEG
  ".jpeg" -> JPEG -- REFACTOR INTO GUARD
  _ -> PLAIN

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
