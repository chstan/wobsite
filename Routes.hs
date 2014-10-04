{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, GADTs,
ExistentialQuantification #-}

module Routes
       (Route,
        match,
        matchPredicate,
        matchNone,
        capture,
        choice,
        zero,
        runRoute
        ) where

import Control.Monad           (msum, join)
import Control.Monad.Free      (Free(Pure, Free), liftF)
import Text.Show.Functions     ()

import Aux

data RouteAtom a where
  MatchPredicate :: ([String] -> ([String], Bool)) -> a -> RouteAtom a
  Capture :: (String -> Maybe a) -> RouteAtom a
  Choice :: [a] -> RouteAtom a
  Zero :: RouteAtom a
  deriving (Functor)

type Route = Free RouteAtom

match :: String -> Route ()
match p = matchPredicate $ pairFn tailSafe (oneAndMatches p)

matchNone :: Route ()
matchNone = matchPredicate $ pairFn id null

matchPredicate :: ([String] -> ([String], Bool)) -> Route ()
matchPredicate p =
  liftF $ MatchPredicate p ()

capture :: (String -> Maybe a) -> Route a
capture convert =
  liftF (Capture convert)

choice :: [Route a] -> Route a
choice a = join $ liftF (Choice a)

zero :: Route a
zero = liftF Zero

runRoute :: Route a -> [String] -> Maybe a
runRoute (Pure a) _ = Just a
runRoute (Free (MatchPredicate p (Pure a))) []
  | (snd . p) [] = Just a
runRoute (Free (MatchPredicate p r)) list
  | (snd . p) list = runRoute r $ fst . p $ list
  | otherwise = Nothing
runRoute (Free (Capture convert)) (p:ps) =
  case convert p of
   Nothing -> Nothing
   (Just r) -> runRoute r ps
runRoute (Free (Choice choices)) paths =
  msum $ map (flip runRoute paths) choices
runRoute (Free Zero) _ = Nothing
runRoute _ [] = Nothing
