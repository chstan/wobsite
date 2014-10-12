module DOM
       () where

import Data.Tree
import Data.Char (intToDigit)
import Control.Monad (when, fail)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language

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

parseHTMLTag :: Parser DOM
parseHTMLTag = do
  attributes <- parseTagStart "html"
  children <- many parseDOM
  parseTagEnd "html"
  return $ Node (HTML attributes) children

parseNavTag :: Parser DOM
parseNavTag = do
  attributes <- parseTagStart "nav"
  children <- many parseDOM
  parseTagEnd "nav"
  return $ Node (NAV attributes) children

parseDivTag :: Parser DOM
parseDivTag = do
  attributes <- parseTagStart "div"
  children <- many parseDOM
  parseTagEnd "div"
  return $ Node (DIV attributes) children

parseBodyTag :: Parser DOM
parseBodyTag = do
  attributes <- parseTagStart "body"
  children <- many parseDOM
  parseTagEnd "body"
  return $ Node (BODY attributes) children

parseHeadTag :: Parser DOM
parseHeadTag = do
  attributes <- parseTagStart "head"
  children <- many parseDOM
  parseTagEnd "head"
  return $ Node (HEAD attributes) children

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

parseOListTag :: Parser DOM
parseOListTag = do
  attributes <- parseTagStart "ol"
  children <- many parseDOM
  parseTagEnd "ol"
  return $ Node (OLIST attributes) children

parseUListTag :: Parser DOM
parseUListTag = do
  attributes <- parseTagStart "ul"
  children <- many parseDOM
  parseTagEnd "ul"
  return $ Node (ULIST attributes) children

parseListElemTag :: Parser DOM
parseListElemTag = do
  attributes <- parseTagStart "li"
  children <- many parseDOM
  parseTagEnd "li"
  return $ Node (LISTELEM attributes) children

parseTitleTag :: Parser DOM
parseTitleTag = do
  parseTagStart "title"
  children <- many parseInlineDOM
  parseTagEnd "title"
  return $ Node TITLE children

parseAnchorTag :: Parser DOM
parseAnchorTag = do
  attributes <- parseTagStart "a"
  children <- many parseDOM
  parseTagEnd "a"
  return $ Node (ANCHOR attributes) children

parsePTag :: Parser DOM
parsePTag = do
  attributes <- parseTagStart "p"
  children <- many parseInlineDOM
  parseTagEnd "p"
  return $ Node (PARAGRAPH attributes) children

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

--parseInlineDOM :: Parser DOM

parseDOM :: Parser DOM
parseDOM =  try parseBlockDOM <|> try parseInlineDOM
