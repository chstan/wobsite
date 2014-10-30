module ResponseRequest
       ( RequestType (GET, POST, PUT, DELETE)
       , PathType (RawPath, ProcessedPath)
       , raw
       , processed
       , Request (Request)
       , ContentDescType (HTML,
                          PLAIN,
                          CSS,
                          PDF,
                          JPEG,
                          PNG)
       , EncodingType (UNZIP,
                       GZIP)
       , Response (Response)
       , rtype
       , path
       , options
       , version
       , statuscode
       , encoding
       , contentDescription
       , body) where

import qualified Data.Map as Map
import Data.ByteString.Lazy.Char8 as BSL8

data PathType = RawPath { raw :: String }
              | ProcessedPath { processed :: [String] } deriving (Show)

data RequestType = GET | POST | PUT | DELETE deriving (Show)
data Request = Request { rtype :: RequestType,
                         path :: PathType,
                         options :: Map.Map String String } deriving (Show)

data EncodingType = UNZIP | GZIP
instance Show EncodingType where
  show t = case t of
    UNZIP -> "identity"
    GZIP -> "gzip"

data ContentDescType = HTML | PLAIN | CSS | PDF | JPEG | PNG
instance Show ContentDescType where
  show c = case c of
    HTML -> "text/html"
    PLAIN -> "text/plain"
    CSS -> "text/css"
    PDF -> "application/pdf"
    JPEG -> "image/jpeg"
    PNG -> "image/png"

data Response = Response { version :: String,
                           statuscode :: Int,
                           encoding :: EncodingType,
                           contentDescription :: ContentDescType,
                           body :: ByteString }

instance Show Response where
  show resp = version(resp) ++ " " ++ show(statuscode(resp)) ++
              " " ++ (case(statuscode(resp)) of
    100 -> "Continue"
    200 -> "OK"
    404 -> "Not Found") ++ "\r\n" ++
              "Content-Type: " ++ show(contentDescription(resp)) ++ "\r\n" ++
              "Content-Length: " ++ show(BSL8.length $ body resp) ++ "\r\n" ++
              "Content-Encoding: " ++ show (encoding resp) ++ "\r\n" ++
              "\r\n" ++ (unpack $ body resp) ++ "\r\n"
