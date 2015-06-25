{-# LANGUAGE OverloadedStrings #-}
module Middleware
       ( breakPath
       , compressResponse
       , cacheImages
       , extractPostParams
       , breakQueryString) where

import qualified Data.Map as Map
import Data.List.Split   (splitOn)
import qualified Codec.Compression.GZip as GZip
import Network.HTTP.Types.URI (urlDecode)
import qualified Data.ByteString.Char8 as BS8

import ResponseRequest

parseArgPair :: String -> (String, String)
parseArgPair s = (var, val)
  where var = head ps
        val = (BS8.unpack . (urlDecode True) . BS8.pack . head . tail) ps
        ps = splitOn "=" s

extractPostParams :: Request -> Request
extractPostParams req = case (rtype req) of
  GET -> req
  POST -> case Map.lookup "Body" (options req) of
    Nothing -> req
    Just b -> req { options = Map.delete "Body" (options req),
                    queryParameters = Map.union (Map.fromList args) oldPs }
      where oldPs = (queryParameters req)
            args = map parseArgPair $ splitOn "&" b

breakQueryString :: Request -> Request
breakQueryString req = case path(req) of
  ProcessedPath _ -> req -- improper use pattern
  RawPath p -> case length (splitOn "?" p) of
    2 -> req {path = RawPath actualPath,
              queryParameters = Map.union oldPs
                                (Map.fromList args) }
    _ -> req
    where oldPs = (queryParameters req)
          args = map parseArgPair (splitOn "&" queryArgs)
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
