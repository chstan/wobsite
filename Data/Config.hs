module Data.Config
       ( ServerEnvironment (..)
       , ConfigurationType (..)
       , EnvironmentType (Development, Production)
       , envAndPortFromLines) where

import Control.Monad (liftM5)
import Control.Concurrent.STM.TVar
import Data.Map as Map
import Safe (readMay)

import Data.Queryable (QueryableHandle)

data EnvironmentType = Development | Production deriving (Show, Read, Eq)

data ServerEnvironment = Environment { engineCommand :: String,
                                       schemeCommand :: String,
                                       engineRecordPath :: String,
                                       dataPath :: String,
                                       envType :: EnvironmentType } deriving (Show)

envAndPortFromLines :: [String] -> (Maybe ServerEnvironment, Maybe Int)
envAndPortFromLines (dev:pn:cc:sc:rp:od:[]) =
  (liftM5 Environment (Just cc) (Just sc) (Just rp) (Just od) (readMay dev), readMay pn)
envAndPortFromLines _ = (Nothing, Nothing)


data ConfigurationType =
  ConfigurationType { engineHandles :: TVar (Map.Map String QueryableHandle),
                      serverEnv :: ServerEnvironment }

instance Show ConfigurationType where show _ = ""
