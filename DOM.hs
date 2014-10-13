module DOM
       () where

import Data.Tree
import Data.Char (intToDigit)
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as Token

import Aux (dummyParam)

type DOM = Tree HTMLNode

type NodeAttribute = [(String, String)]

data HTMLNode = ROOT           NodeAttribute
              | DOCTYPE String

              | HTML           NodeAttribute
              | TITLE
              | BOLD           NodeAttribute
              | ITALIC         NodeAttribute
              | ULIST          NodeAttribute
              | OLIST          NodeAttribute
              | LISTELEM       NodeAttribute
              | LINK           NodeAttribute
              | HEAD           NodeAttribute
              | BODY           NodeAttribute

              | ANCHOR         NodeAttribute
              | PARAGRAPH      NodeAttribute
              | HEADING Int    NodeAttribute
              | DIV            NodeAttribute
              | NAV            NodeAttribute
              | ARTICLE        NodeAttribute
              | TEXT String
              | CUSTOM String  NodeAttribute
              | EMPTY
                deriving (Show)

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
parseTitleTag = parseUniformTag "title" (dummyParam TITLE)
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
       return $ Node (TEXT (foldl (++) [] text)) children

parseInlineDOM :: Parser DOM
parseInlineDOM = try parseText

parseDocType :: Parser DOM
parseDocType = do
  try (string "<DOCTYPE") <|> string "<!DOCTYPE"
  spaces
  doc_type <- Token.identifier attributeLexer
  char '>'
  children <- many parseDOM
  return $ Node (DOCTYPE doc_type) children

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
