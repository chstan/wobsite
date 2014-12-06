module Data.Config
       ( ServerEnvironment (Environment)
       , engineCommand
       , engineRecordPath
       , ConfigurationType (ConfigurationType)
       , engineHandles
       , serverEnv

       , envAndPortFromLines) where

import Control.Concurrent.STM.TVar
import Data.Map as Map
import Safe (readMay)

import Data.Chess

data ServerEnvironment = Environment { engineCommand :: String,
                                       engineRecordPath :: String } deriving (Show)

envAndPortFromLines :: [String] -> (Maybe ServerEnvironment, Maybe Int)
envAndPortFromLines (pn:cc:rp:[]) =
  (Just $ Environment cc rp, readMay pn)
envAndPortFromLines _ = (Nothing, Nothing)


data ConfigurationType =
  ConfigurationType { engineHandles :: TVar (Map.Map String ChessEngineHandle),
                      serverEnv :: ServerEnvironment }

instance Show ConfigurationType where show _ = ""
