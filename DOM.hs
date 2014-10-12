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
              | TEXT String
              | CUSTOM String  NodeAttribute
              | EMPTY
                deriving (Show)

uniformConsFromName :: String -> NodeAttribute -> HTMLNode
uniformConsFromName "html" = HTML
uniformConsFromName "ul" = ULIST
uniformConsFromName "ol" = OLIST
uniformConsFromName "li" = LISTELEM
uniformConsFromName "head" = HEAD
uniformConsFromName "body" = BODY
uniformConsFromName "a" = ANCHOR
uniformConsFromName "p" = PARAGRAPH
uniformConsFromName "div" = DIV
uniformConsFromName "nav" = NAV
uniformConsFromName "title" = dummyParam TITLE

parseUniformTag :: String -> Parser DOM
parseUniformTag tag_label = do
  attributes <- parseTagStart tag_label
  children <- many parseDOM
  parseTagEnd tag_label
  return $ Node (uniformConsFromName tag_label
                attributes) children

parseHTMLTag = parseUniformTag "html"
parseUListTag = parseUniformTag "ul"
parseOListTag = parseUniformTag "ol"
parseListElemTag = parseUniformTag "li"
parseHeadTag = parseUniformTag "head"
parseBodyTag = parseUniformTag "body"
parseAnchorTag = parseUniformTag "a"
parsePTag = parseUniformTag "p"
parseDivTag = parseUniformTag "div"
parseNavTag = parseUniformTag "nav"
parseTitleTag = parseUniformTag "title"

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
    Token.identStart = alphaNum <|> oneOf "?=#-:,+_/\\.>",
    Token.identLetter = alphaNum <|> oneOf " ?=#-:,+_/\\>.",
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
                try parseNavTag <|> try parseAnchorTag

parseDOM :: Parser DOM
parseDOM =  try parseBlockDOM <|> try parseInlineDOM
