module Lexer where

import Text.Parsec.String(Parser)
import Text.Parsec.Language(emptyDef)

import qualified Text.Parsec.Token as Token

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
    opeators = ["+","-","*","/"]
    keywords = ["val", "fn", "ext_c"]
    style = emptyDef
             { Token.commentLine = "--"
             , Token.reservedOpNames = opeators
             , Token.reservedNames = keywords
             }

integer :: Parser Integer
integer = Token.integer lexer

float :: Parser Double
float = Token.float lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

operator :: String -> Parser ()
operator = Token.reservedOp lexer

