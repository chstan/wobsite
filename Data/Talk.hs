{-# LANGUAGE DeriveGeneric #-}

module Data.Talk
       (TalkRecord (TalkRecord)
       ,name
       ,date
       ,location
       ,presentation_title
       ,presentation_url
       ,invited) where

import GHC.Generics

import Data.Aeson
import Data.Text

data TalkRecord =
  TalkRecord { name :: !Text,
               date :: !Text,
               location :: !Text,
               presentation_title :: !Text,
               presentation_url :: !Text,
               invited :: Bool
             } deriving (Show, Generic)

instance FromJSON TalkRecord
instance ToJSON TalkRecord
