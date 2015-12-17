module Lexer where

import Text.Parsec.String(Parser)
import Text.Parsec.Language(emptyDef)

import qualified Text.Parsec.Token as Token

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
    opeators = [ "+", "-", "*", "/", "&", "|"
               , "==", "/=", "<", "<=", ">", ">=", "[", "]", "^"
               ]
    keywords = ["val", "mutval", "fn", "ext_c", "ret", "if", "not"]
    style = emptyDef
             { Token.commentLine = "--"
             , Token.reservedOpNames = opeators
             , Token.reservedNames = keywords
             , Token.caseSensitive = True
             }

charLiteral :: Parser Char
charLiteral = Token.charLiteral lexer

integer :: Parser Integer
integer = Token.integer lexer

float :: Parser Double
float = Token.float lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

operator :: String -> Parser ()
operator = Token.reservedOp lexer

