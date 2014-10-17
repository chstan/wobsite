{-# LANGUAGE DeriveGeneric #-}

module Data.Book
       (BookRecord(BookRecord)
       ,title
       ,author
       ,completion_date
       ,impression
       ,finished) where

import GHC.Generics

import Data.Aeson
import Data.Text

data BookRecord =
  BookRecord { title :: !Text,
               author :: !Text,
               completion_date :: !Text,
               impression :: !Text,
               finished :: Bool
             } deriving (Show, Generic)

instance FromJSON BookRecord
instance ToJSON BookRecord
