{-# LANGUAGE TypeSynonymInstances #-}
module DOM
       (DOM,
        HTMLNode (..),
        NodeAttribute,
        showDOM,
        doParseDOM,
        ) where

import Data.Tree
import Data.Char (intToDigit)
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as Token

import Aux (dummyParam)

type DOM = Tree HTMLNode

type NodeAttribute = [(String, String)]

data HTMLNode = DOCTYPE  { document_type :: String ,attributes :: NodeAttribute}
              | HTML           { attributes :: NodeAttribute }
              | TITLE          { attributes :: NodeAttribute }
              | BOLD           { attributes :: NodeAttribute }
              | ITALIC         { attributes :: NodeAttribute }
              | ULIST          { attributes :: NodeAttribute }
              | OLIST          { attributes :: NodeAttribute }
              | LISTELEM       { attributes :: NodeAttribute }
              | LINK           { attributes :: NodeAttribute }
              | HEAD           { attributes :: NodeAttribute }
              | BODY           { attributes :: NodeAttribute }

              | ANCHOR         { attributes :: NodeAttribute }
              | PARAGRAPH      { attributes :: NodeAttribute }
              | HEADING        { n :: Int, attributes :: NodeAttribute }
              | DIV            { attributes :: NodeAttribute }
              | NAV            { attributes :: NodeAttribute }
              | ARTICLE        { attributes :: NodeAttribute }
              | TEXT           { content :: String, attributes :: NodeAttribute }

doParseDOM :: String -> Either ParseError DOM
doParseDOM = (parse parseDOM "Parsing failed.")

showDOM :: DOM -> String
showDOM (Node t f) = (showOpeningTag t) ++ (concat $ map showDOM f) ++ (showClosingTag t)

showNodeAttribute :: NodeAttribute -> String
showNodeAttribute [] = ""
showNodeAttribute ((t,v):xs) = " " ++ t ++ "=" ++ "\"" ++
                               v ++ "\"" ++ showNodeAttribute xs

showOpeningTag :: HTMLNode -> String
showOpeningTag (DOCTYPE d _) = "<DOCTYPE " ++ d ++ ">"
showOpeningTag (LINK attrs) = "<link" ++ showNodeAttribute attrs ++ ">"
showOpeningTag (HTML attrs) = "<html" ++ showNodeAttribute attrs ++ ">"
showOpeningTag (TITLE _) = "<title>"
showOpeningTag (BOLD attrs) = "<b" ++ showNodeAttribute attrs ++ ">"
showOpeningTag (ITALIC attrs) = "<i" ++ showNodeAttribute attrs ++ ">"
showOpeningTag (ULIST attrs) = "<ul" ++ showNodeAttribute attrs ++ ">"
showOpeningTag (OLIST attrs) = "<ol" ++ showNodeAttribute attrs ++ ">"
showOpeningTag (LISTELEM attrs) = "<li" ++ showNodeAttribute attrs ++ ">"
showOpeningTag (HEAD attrs) = "<head" ++ showNodeAttribute attrs ++ ">"
showOpeningTag (BODY attrs) = "<body" ++ showNodeAttribute attrs ++ ">"
showOpeningTag (ANCHOR attrs) = "<a" ++ showNodeAttribute attrs ++ ">"
showOpeningTag (PARAGRAPH attrs) = "<p" ++ showNodeAttribute attrs ++ ">"
showOpeningTag (HEADING n attrs) = "<h" ++ show n ++ " " ++ showNodeAttribute attrs ++ ">"
showOpeningTag (DIV attrs) = "<div" ++ showNodeAttribute attrs ++ ">"
showOpeningTag (NAV attrs) = "<nav" ++ showNodeAttribute attrs ++ ">"
showOpeningTag (ARTICLE attrs) = "<article" ++ showNodeAttribute attrs ++ ">"
showOpeningTag (TEXT s _) = s

showClosingTag :: HTMLNode -> String
showClosingTag (DOCTYPE _ _) = ""
showClosingTag (LINK _) = ""
showClosingTag (HTML _) = "</html>"
showClosingTag (TITLE _) = "</title>"
showClosingTag (BOLD _) = "</b>"
showClosingTag (ITALIC _) = "</i>"
showClosingTag (ULIST _) = "</ul>"
showClosingTag (OLIST _) = "</ol>"
showClosingTag (LISTELEM _) = "</li>"
showClosingTag (HEAD _) = "</head>"
showClosingTag (BODY _) = "</body>"
showClosingTag (ANCHOR _) = "</a>"
showClosingTag (PARAGRAPH _) = "</p>"
showClosingTag (HEADING n _) = "</h" ++ show n ++ ">"
showClosingTag (DIV _) = "</div>"
showClosingTag (NAV _) = "</nav>"
showClosingTag (ARTICLE _) = "</article>"
showClosingTag (TEXT _ _) = ""


parseUniformTag :: String -> (NodeAttribute -> HTMLNode)
                   -> Parser DOM
parseUniformTag tag_label c = do
  attributes <- parseTagStart tag_label
  children <- many parseDOM
  parseTagEnd tag_label
  return $ Node (c attributes) children

parseHTMLTag = parseUniformTag "html" HTML
parseUListTag = parseUniformTag "ul" ULIST
parseOListTag = parseUniformTag "ol" OLIST
parseListElemTag = parseUniformTag "li" LISTELEM
parseHeadTag = parseUniformTag "head" HEAD
parseBodyTag = parseUniformTag "body" BODY
parseAnchorTag = parseUniformTag "a" ANCHOR
parsePTag = parseUniformTag "p" PARAGRAPH
parseDivTag = parseUniformTag "div" DIV
parseNavTag = parseUniformTag "nav" NAV
parseTitleTag = parseUniformTag "title" TITLE
parseBoldTag = parseUniformTag "b" BOLD
parseItalicTag = parseUniformTag "i" ITALIC
parseArticleTag = parseUniformTag "article" ARTICLE

attributeLexerStyle :: Token.LanguageDef ()
attributeLexerStyle = Token.LanguageDef
  {
    Token.commentStart = "",
    Token.commentEnd = "",
    Token.commentLine = "",
    Token.nestedComments = False,
    Token.identStart = alphaNum <|> oneOf "?=#-:,+_/\\.",
    Token.identLetter = alphaNum <|> oneOf " ?=#-:,+_/\\.",
    Token.opStart = oneOf "=",
    Token.opLetter = oneOf "=",
    Token.reservedNames = [],
    Token.reservedOpNames = [],
    Token.caseSensitive = True
  }

textLexerStyle :: Token.LanguageDef ()
textLexerStyle = Token.LanguageDef
  {
    Token.commentStart = "",
    Token.commentEnd = "",
    Token.commentLine = "",
    Token.nestedComments = False,
    Token.identStart = alphaNum <|> oneOf " !?=#-':,+_/\\.>();&{}",
    Token.identLetter = alphaNum <|> oneOf " !?=#-':,+_/\\>.();&{}",
    Token.opStart = oneOf "<",
    Token.opLetter = oneOf "</>",
    Token.reservedNames = [],
    Token.reservedOpNames = [],
    Token.caseSensitive = True
  }

attributeLexer :: Token.TokenParser ()
attributeLexer = Token.makeTokenParser attributeLexerStyle

textLexer :: Token.TokenParser ()
textLexer = Token.makeTokenParser textLexerStyle

parseAttribute :: Parser (String, String)
parseAttribute = do
  spaces
  attr_name <- many alphaNum
  spaces
  char '='
  spaces
  char '\"'
  attr_value <- Token.identifier attributeLexer
  char '\"'
  spaces
  return (attr_name, attr_value)

parseStandaloneTag :: String -> Parser NodeAttribute
parseStandaloneTag tag_label = do
  spaces
  char '<'
  string tag_label
  attributes <- many parseAttribute
  char '>'
  return attributes

parseTagStart :: String -> Parser NodeAttribute
parseTagStart tag_label = do
  spaces
  char '<'
  string tag_label
  attributes <- many parseAttribute
  char '>'
  return attributes

parseTagEnd :: String -> Parser ()
parseTagEnd tag_label = do
  spaces
  string $ "</" ++ tag_label ++ ">"
  return ()

parseLinkTag :: Parser DOM
parseLinkTag = do
  attributes <- parseStandaloneTag "link"
  children <- many parseDOM
  return $ Node (LINK attributes) children

parseHeadingNumber :: Parser Int
parseHeadingNumber = do
  n <- oneOf "123456"
  return (read [n])

parseHeadingTagStart :: Parser (Int, NodeAttribute)
parseHeadingTagStart = do
  spaces
  string "<h"
  n <- parseHeadingNumber
  attributes <- many parseAttribute
  string ">"
  return (n, attributes)

parseHeadingTagEnd :: Int -> Parser ()
parseHeadingTagEnd n = do
  string "</h"
  char $ intToDigit n
  char '>'
  return ()

parseHeadingTag :: Parser DOM
parseHeadingTag = do
  (n, attributes) <- parseHeadingTagStart
  children <- many parseDOM
  parseHeadingTagEnd n
  return $ Node (HEADING n attributes) children

parseText :: Parser DOM
parseText = do
  text <- many $ Token.identifier textLexer
  case null text of
     True -> fail "No text encountered in parseText"
     False -> do
       children <- many parseInlineDOM
       return $ Node (TEXT (foldl (++) [] text) []) children

parseInlineDOM :: Parser DOM
parseInlineDOM = try parseText

parseDocType :: Parser DOM
parseDocType = do
  try (string "<DOCTYPE") <|> string "<!DOCTYPE"
  spaces
  doc_type <- Token.identifier attributeLexer
  char '>'
  children <- many parseDOM
  return $ Node (DOCTYPE doc_type []) children

parseBlockDOM :: Parser DOM
parseBlockDOM = try parseDivTag <|> try parseHTMLTag <|>
                try parseHeadTag <|> try parseBodyTag <|>
                try parseHeadingTag <|> try parsePTag <|>
                try parseDocType <|> try parseLinkTag <|>
                try parseTitleTag <|> try parseOListTag <|>
                try parseUListTag <|> try parseListElemTag <|>
                try parseNavTag <|> try parseAnchorTag <|>
                try parseArticleTag <|> try parseBoldTag <|>
                try parseItalicTag

parseDOM :: Parser DOM
parseDOM =  try parseBlockDOM <|> try parseInlineDOM
