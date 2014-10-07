module Middleware
       (breakPath) where

import Data.List.Split   (splitOn)
import ResponseRequest

breakPath :: Request -> Request
breakPath req = case path(req) of
  ProcessedPath _ -> req
  RawPath p -> req {path = ProcessedPath $ filter (not . null) $ splitOn "/" p }
