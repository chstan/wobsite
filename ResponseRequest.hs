module ResponseRequest
       ( RequestType (GET, POST, PUT, DELETE)
       , PathType (RawPath, ProcessedPath)
       , raw
       , processed

       , Request (Request)
       , isProduction
       , ContentDescType (HTML,
                          PLAIN,
                          CSS,
                          PDF,
                          JPEG,
                          PNG,
                          TAR,
                          JSON,
                          JS)
       , inferContentDescType

       , EncodingType (UNZIP,
                       GZIP)

       , CachingInfo (Dynamic,
                      Cacheable)

       , CacheHeader (Public,
                      Private,
                      Nocache,
                      MaxAge)
       , Response (Response)
       , rtype
       , path
       , serverConfig
       , options
       , version
       , queryParameters
       , statuscode
       , encoding
       , cachingInfo
       , contentDescription
       , body

       , setCaching) where

import qualified Data.Map as Map
import Data.ByteString.Lazy.Char8 as BSL8
import Data.Config
import Data.List (intercalate)
import System.FilePath (takeExtension)

data PathType = RawPath { raw :: String }
              | ProcessedPath { processed :: [String] } deriving (Show)

data RequestType = GET | POST | PUT | DELETE deriving (Show)
data Request = Request { rtype :: RequestType,
                         path :: PathType,
                         serverConfig :: ConfigurationType,
                         queryParameters :: Map.Map String String,
                         options :: Map.Map String String } deriving (Show)

isProduction :: Request -> Bool
isProduction r = ((== Production) . envType . serverEnv . serverConfig) r

data EncodingType = UNZIP | GZIP
instance Show EncodingType where
  show t = case t of
    UNZIP -> "identity"
    GZIP -> "gzip"

data ContentDescType = HTML | PLAIN | CSS | PDF | JPEG | PNG | TAR | JSON | JS
                     deriving (Eq)

instance Show ContentDescType where
  show c = case c of
    HTML -> "text/html"
    PLAIN -> "text/plain"
    CSS -> "text/css"
    PDF -> "application/pdf"
    JPEG -> "image/jpeg"
    PNG -> "image/png"
    TAR -> "application/x-tar"
    JSON -> "application/json"
    JS -> "text/javascript"
    --_ -> "text/plain"

inferContentDescType :: String -> ContentDescType
inferContentDescType loc = case takeExtension loc of
  ".css" -> CSS
  ".html" -> HTML
  ".pdf" -> PDF
  ".png" -> PNG
  ".jpg" -> JPEG
  ".jpeg" -> JPEG -- REFACTOR INTO GUARD
  ".tar" -> TAR
  ".json" -> JSON
  ".js" -> JS
  _ -> PLAIN


data CacheHeader = Public | Private | Nocache | MaxAge Int
instance Show CacheHeader where
  show Public = "public"
  show Private = "private"
  show Nocache = "no-cache"
  show (MaxAge age) = "max-age=" ++ (show age)

data CachingInfo = Dynamic | Cacheable [CacheHeader]
instance Show CachingInfo where
  show Dynamic = "X-Rack-Cache: miss\r\n" ++
                 "Cache-Control: max-age=0, no-cache, no-store\r\n" ++
                 "Pragma: no-cache\r\n"
  show (Cacheable []) = ""
  show (Cacheable ls) = "Cache-Control: " ++
                        (Data.List.intercalate ", " $ fmap show ls) ++ "\r\n"

data Response = Response { version :: String,
                           statuscode :: Int,
                           encoding :: EncodingType,
                           contentDescription :: ContentDescType,
                           cachingInfo :: CachingInfo,
                           body :: ByteString }

instance Show Response where
  show resp = version(resp) ++ " " ++ show(statuscode(resp)) ++
              " " ++ (case(statuscode(resp)) of
    100 -> "Continue"
    200 -> "OK"
    404 -> "Not Found") ++ "\r\n" ++
              "Content-Type: " ++ show(contentDescription(resp)) ++ "\r\n" ++
              "Content-Length: " ++ show(BSL8.length $ body resp) ++ "\r\n" ++
              "Content-Encoding: " ++ show(encoding resp) ++ "\r\n" ++
              "Connection: " ++ "close" ++ "\r\n" ++
              show(cachingInfo resp) ++
              "\r\n" ++ (unpack $ body resp) ++ "\r\n"

setCaching :: Response -> Response
setCaching (Response v s e cd _ b) = Response v s e cd
                                     (Cacheable [Public, MaxAge (60 * 60 * 3)])
                                     b
