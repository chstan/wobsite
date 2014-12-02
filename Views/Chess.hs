{-# LANGUAGE OverloadedStrings #-}

module Views.Chess
       (chessJSONView
       ) where

chessJSONView :: String -> [String] -> String
chessJSONView bestMove _ =
--chessJSONView bestReply allStrings =
  "{\"hello\":\"\"" ++
  ",\"bestMove\":\"" ++ bestMove ++ "\"}"
