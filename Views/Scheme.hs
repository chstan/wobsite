module Views.Scheme
       (schemeJSONView) where

import Data.List (intercalate)

schemeJSONView :: [String] -> String
schemeJSONView [] = "{}"
schemeJSONView ls = "{\"out\":[" ++
  (intercalate "," (fmap (\x -> ("\"" ++ x ++ "\"")) ls)) ++ "]}"
