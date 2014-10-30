module Middleware
       (breakPath,
        compressResponse
        ) where

import qualified Data.Map as Map
import Data.List.Split   (splitOn)
import qualified Codec.Compression.GZip as GZip

import ResponseRequest

breakPath :: Request -> Request
breakPath req = case path(req) of
  ProcessedPath _ -> req
  RawPath p -> req {path = ProcessedPath $ filter (not . null) $ splitOn "/" p }

compressResponse :: Request -> Response -> Response
compressResponse req resp
  | allowsGzip = resp { encoding = GZIP,
                        body = GZip.compress unzippedBody }
  | otherwise = resp
  where
    allowsGzip = elem "gzip" allowedOptions
    allowedOptions = splitOn ", " allowedOptionsString
    allowedOptionsString = Map.findWithDefault "" "Accept-Encoding" (options req)
    unzippedBody = body resp
