module Views.Scheme
       (schemeJSONView) where

import Data.List (intercalate)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL8

schemeJSONView :: [String] -> String
schemeJSONView [] = "{}"
schemeJSONView ls = "{\"out\":[" ++
  (intercalate "," (fmap (BSL8.unpack . encode . toJSON) ls)) ++ "]}"
