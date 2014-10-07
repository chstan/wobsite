module ResponseRequest
       ( RequestType (GET, POST, PUT, DELETE)
       , PathType (RawPath, ProcessedPath)
       , Request (Request)
       , ContentDescType (TEXT_HTML,
                          TEXT_PLAIN,
                          TEXT_CSS,
                          PDF)
       , Response (Response)
       , rtype
       , path
       , options
       , version
       , statuscode
       , contentDescription
       , body) where

data PathType = RawPath { raw :: String }
              | ProcessedPath { processed :: [String] } deriving (Show)

data RequestType = GET | POST | PUT | DELETE deriving (Show)
data Request = Request { rtype :: RequestType,
                         path :: PathType,
                         options :: [(String, String)] } deriving (Show)

data ContentDescType = TEXT_HTML | TEXT_PLAIN | TEXT_CSS | PDF
instance Show ContentDescType where
  show c = case c of
    TEXT_HTML -> "text/html"
    TEXT_PLAIN -> "text/plain"
    TEXT_CSS -> "text/css"
    PDF -> "application/pdf"

data Response = Response { version :: String,
                           statuscode :: Int,
                           contentDescription :: ContentDescType,
                           body :: String }

instance Show Response where
  show resp = version(resp) ++ " " ++ show(statuscode(resp)) ++
              " " ++ (case(statuscode(resp)) of
    100 -> "Continue"
    200 -> "OK"
    404 -> "Not Found") ++ "\r\n" ++
              "Content-Type: " ++ show(contentDescription(resp)) ++ "\r\n" ++
              "Content-Length: " ++ show(length $ body(resp)) ++ "\r\n" ++
              "\r\n" ++ body(resp) ++ "\r\n"
