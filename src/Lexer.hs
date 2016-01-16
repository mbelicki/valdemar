module Lexer where

import Text.Parsec.String(Parser)
import Text.Parsec.Language(emptyDef)

import qualified Text.Parsec.Token as Token
import qualified Text.Parsec.Combinator as Combinator
import qualified Text.Parsec.Char as Char

import qualified Data.List as List

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
    opeators = [ "+", "-", "*", "/", "&", "|"
               , "==", "/=", "<", "<=", ">", ">=", "[", "]", "^"
               ]
    keywords = ["val", "mutval", "fn", "ext_c", "ret", "if", "not", "tuple"]
    style = emptyDef
             { Token.commentLine = "--"
             , Token.reservedOpNames = opeators
             , Token.reservedNames = keywords
             , Token.caseSensitive = True
             }

isValidTypeName :: String -> Bool
isValidTypeName n = "_t" `List.isSuffixOf` n

charLiteral :: Parser Char
charLiteral = Token.charLiteral lexer

integer :: Parser Integer
integer = Token.integer lexer

float :: Parser Double
float = Token.float lexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

commaSepNoDangling :: Parser a -> Parser [a]
commaSepNoDangling p = Combinator.sepBy p (Token.comma lexer)

commaSep :: Parser a -> Parser [a]
commaSep p = Combinator.sepEndBy p (Token.comma lexer)

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

operator :: String -> Parser ()
operator = Token.reservedOp lexer

