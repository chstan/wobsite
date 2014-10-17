{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Data.Project
       (ProjectSynopsis(ProjectSynopsis)
       ,title
       ,label
       ,picture_file
       ,description
       ) where

import GHC.Generics

import Data.Aeson
import Data.Text

data ProjectSynopsis =
  ProjectSynopsis { title :: !Text,
                    label :: !Text,
                    picture_file :: !Text,
                    description :: !Text
                  } deriving (Show, Generic)

instance FromJSON ProjectSynopsis
instance ToJSON ProjectSynopsis
