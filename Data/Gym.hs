{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Data.Gym
       (Weight(..)
       ,WeightMap
       ,Exercise(..)
       ,ExerciseMap
       ) where

import GHC.Generics
import qualified Data.Map as Map

import Data.Aeson

data Weight =
  Weight { body_weight :: Float
         } deriving (Show, Generic)

instance FromJSON Weight
instance ToJSON Weight

data Exercise =
  Exercise { weight :: Maybe Float,
             repetitions :: Int,
             sets :: Int
           } deriving (Show, Generic)

instance FromJSON Exercise
instance ToJSON Exercise

type ExerciseMap = Map.Map String (Map.Map String Exercise)
type WeightMap = Map.Map String Weight
