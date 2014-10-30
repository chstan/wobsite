{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Data.BlogEntry
       (BlogListing(BlogListing)
       ,title
       ,label
       ,markdown_location
       ,picture_file
       ,description
       ,lookupBlogEntry
       ) where

import GHC.Generics

import Data.Aeson
import Data.Text
import Data.List (find)

data BlogListing =
  BlogListing { title :: !Text,
                label :: !Text,
                markdown_location :: !Text,
                picture_file :: !Text,
                description :: !Text
              } deriving (Show, Generic)

lookupBlogEntry :: String -> Either a [BlogListing] -> Maybe BlogListing
lookupBlogEntry _ (Left _) = Nothing
lookupBlogEntry t (Right l) = Data.List.find (\r -> unpack(label(r)) == t) l


instance FromJSON BlogListing
instance ToJSON BlogListing
