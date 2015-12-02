module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as E
import qualified Text.Parsec.Token as T

import qualified Control.Monad as M

import Lexer
import Syntax

binary symbol function = E.Infix (operator symbol >> return (BinOp function))

table = [ [ binary "*" Mul E.AssocLeft
          , binary "/" Div E.AssocLeft
          ]
        , [ binary "+" Add E.AssocLeft
          , binary "-" Sub E.AssocLeft
          ]
        ]

int :: Parser Expr
int = M.liftM (Integer . fromInteger) integer

floating :: Parser Expr
floating = M.liftM Float float

variable :: Parser Expr
variable = M.liftM Var identifier

funArg :: Parser FunArg
funArg = do
    name <- identifier
    typeName <- identifier
    return $ FunArg name typeName

value :: Parser Expr
value = do
  reserved "val"
  name <- identifier
  typeName <- identifier
  reserved "="
  body <- expr
  return $ ValDecl name typeName body

functionDeclType :: Parser ([FunArg], TypeName)
functionDeclType = do
    args <- parens $ commaSep funArg
    reserved "->"
    retType <- identifier
    return (args, retType)

function :: Parser Expr
function = do
  reserved "fn"
  name <- identifier
  (args, retType) <- functionDeclType
  body <- braces $ many statement
  return $ FunDecl name args retType body

extFunction :: Parser Expr
extFunction = do
  reserved "ext_c"
  name <- identifier
  (args, retType) <- functionDeclType
  return $ ExtFunDecl name args retType

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

anyExpr :: Parser Expr
anyExpr = try floating
      <|> try int
      <|> try value
      <|> try function
      <|> try extFunction
      <|> try call
      <|> variable
      <|> parens expr

expr :: Parser Expr
expr = E.buildExpressionParser table anyExpr

statementReturn :: Parser Statement
statementReturn = do
    reserved "ret" 
    e <- expr
    return $ Return e

statementDecl :: Parser Statement
statementDecl = do
    e <- value <|> function
    return $ DeclStatement e

statementCall :: Parser Statement
statementCall = do
    e <- call
    return $ CallStatement e

statement :: Parser Statement
statement = try statementReturn
        <|> try statementCall
        <|> try statementDecl

contents :: Parser a -> Parser a
contents p = do
  T.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many (function <|> extFunction <|> value)

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseModule :: String -> Either ParseError [Expr]
parseModule = parse (contents toplevel) "<stdin>"

