{-# LANGUAGE OverloadedStrings #-}
module Middleware
       ( breakPath
       , compressResponse
       , cacheImages
       , breakQueryString) where

import qualified Data.Map as Map
import Data.List.Split   (splitOn)
import qualified Codec.Compression.GZip as GZip
import Network.HTTP.Types.URI (urlDecode)
import qualified Data.ByteString.Char8 as BS8

import ResponseRequest

breakQueryString :: Request -> Request
breakQueryString req = case path(req) of
  ProcessedPath _ -> req -- improper use pattern
  RawPath p -> case length (splitOn "?" p) of
    2 -> req {path = RawPath actualPath,
              queryParameters = Map.fromList $ zip argVars argVals }
    _ -> req
    where argVals = map (BS8.unpack . (urlDecode True) . BS8.pack . head . tail) argPairs
          argVars = map head argPairs
          argPairs = map (splitOn "=") $ splitOn "&" queryArgs
          actualPath:queryArgs:[] = splitOn "?" p

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

cacheMimes :: [ContentDescType] -> Response -> Response
cacheMimes ms r =
  case cachingInfo r of
   Dynamic -> r
   _ -> case elem (contentDescription r) ms of
     True -> setCaching r
     False -> r

cacheImages :: Response -> Response
cacheImages = cacheMimes [JPEG, PNG]
