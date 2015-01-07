module Data.Config
       ( ServerEnvironment (Environment)
       , engineCommand
       , engineRecordPath
       , envType
       , ConfigurationType (ConfigurationType)
       , engineHandles
       , serverEnv
       , EnvironmentType (Development, Production)
       , envAndPortFromLines) where

import Control.Monad (liftM3)
import Control.Concurrent.STM.TVar
import Data.Map as Map
import Safe (readMay)

import Data.Chess

data EnvironmentType = Development | Production deriving (Show, Read, Eq)

data ServerEnvironment = Environment { engineCommand :: String,
                                       engineRecordPath :: String,
                                       envType :: EnvironmentType } deriving (Show)

envAndPortFromLines :: [String] -> (Maybe ServerEnvironment, Maybe Int)
envAndPortFromLines (dev:pn:cc:rp:[]) =
  (liftM3 Environment (Just cc) (Just rp) (readMay dev), readMay pn)
envAndPortFromLines _ = (Nothing, Nothing)


data ConfigurationType =
  ConfigurationType { engineHandles :: TVar (Map.Map String ChessEngineHandle),
                      serverEnv :: ServerEnvironment }

instance Show ConfigurationType where show _ = ""
