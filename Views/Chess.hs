{-# LANGUAGE OverloadedStrings #-}

module Views.Chess
       ( chessJSONView
       , chessResultJSONView
       ) where

chessJSONView :: String -> [String] -> String
chessJSONView bestMove _ =
--chessJSONView bestReply allStrings =
  "{\"hello\":\"\"" ++
  ",\"bestMove\":\"" ++ bestMove ++ "\"}"

chessResultJSONView :: String -> String
chessResultJSONView res =
  "{\"resolution\":\"" ++ res ++ "\"}"
