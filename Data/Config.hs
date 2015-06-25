module Data.Config
       ( ServerEnvironment (..)
       , ConfigurationType (..)
       , EnvironmentType (Development, Production)
       , envAndPortFromLines) where

import Control.Concurrent.STM.TVar
import Data.Map as Map
import Safe (readMay)

import Data.Queryable (QueryableHandle)

data EnvironmentType = Development | Production deriving (Show, Read, Eq)

data ServerEnvironment = Environment { engineCommand :: String,
                                       schemeCommand :: String,
                                       dominionCommand :: String,
                                       javaPolicyPath :: String,
                                       engineRecordPath :: String,
                                       dataPath :: String,
                                       envType :: EnvironmentType } deriving (Show)

envAndPortFromLines :: [String] -> (Maybe ServerEnvironment, Maybe Int)
envAndPortFromLines (dev:pn:cc:sc:dc:jp:rp:od:[]) =
  case (readMay dev, readMay pn) of
   (Just jdev, Just jpn) -> (Just $ Environment cc sc dc jp rp od jdev, Just jpn)
   _ -> (Nothing, Nothing)
envAndPortFromLines _ = (Nothing, Nothing)


data ConfigurationType =
  ConfigurationType { engineHandles :: TVar (Map.Map String QueryableHandle),
                      serverEnv :: ServerEnvironment }

instance Show ConfigurationType where
  show ct = show $ serverEnv ct
