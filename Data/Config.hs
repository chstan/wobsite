module Data.Config
       ( ServerEnvironment (..)
       , ConfigurationType (..)
       , EnvironmentType (Development, Production)
       , envAndPortFromLines) where

import Control.Monad (liftM4)
import Control.Concurrent.STM.TVar
import Data.Map as Map
import Safe (readMay)

import Data.Chess

data EnvironmentType = Development | Production deriving (Show, Read, Eq)

data ServerEnvironment = Environment { engineCommand :: String,
                                       engineRecordPath :: String,
                                       dataPath :: String,
                                       envType :: EnvironmentType } deriving (Show)

envAndPortFromLines :: [String] -> (Maybe ServerEnvironment, Maybe Int)
envAndPortFromLines (dev:pn:cc:rp:od:[]) =
  (liftM4 Environment (Just cc) (Just rp) (Just od) (readMay dev), readMay pn)
envAndPortFromLines _ = (Nothing, Nothing)


data ConfigurationType =
  ConfigurationType { engineHandles :: TVar (Map.Map String ChessEngineHandle),
                      serverEnv :: ServerEnvironment }

instance Show ConfigurationType where show _ = ""
