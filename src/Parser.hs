module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as E
import qualified Text.Parsec.Token as T

import qualified Control.Monad as M

import Lexer
import Syntax

binary symbol function = E.Infix (operator symbol >> return (BinOpExpr function))

table = [ [ binary "*" Mul E.AssocLeft
          , binary "/" Div E.AssocLeft
          ]
        , [ binary "+" Add E.AssocLeft
          , binary "-" Sub E.AssocLeft
          ]
        , [ binary "&" And E.AssocLeft ]
        , [ binary "|" Or E.AssocLeft ]
        ]

int :: Parser Expression
int = M.liftM (IntegerExpr . fromInteger) integer

floating :: Parser Expression
floating = M.liftM FloatExpr float

boolValue :: Parser Bool
boolValue = (reserved "true"  >> return True)
        <|> (reserved "false" >> return False)

boolean :: Parser Expression
boolean = M.liftM BooleanExpr boolValue

variable :: Parser Expression
variable = M.liftM VarExpr identifier

funArg :: Parser FunctionArgument
funArg = do
    name <- identifier
    typeName <- identifier
    return $ FunArg name typeName

valueDecl :: Parser ValueDeclaration
valueDecl = do
    reserved "val"
    name <- identifier
    typeName <- identifier
    reserved "="
    body <- expr
    return $ ValDecl name typeName body

value :: Parser Expression
value = M.liftM ValDeclExpr valueDecl

functionDecl :: Parser FunctionDeclaration
functionDecl = do
    name <- identifier
    args <- parens $ commaSep funArg
    reserved "->"
    retType <- identifier
    return $ FunDecl name args retType

function :: Parser Expression
function = do
  reserved "fn"
  decl <- functionDecl
  body <- statement
  return $ FunDeclExpr decl body

extFunction :: Parser Expression
extFunction = do
  reserved "ext_c"
  decl <- functionDecl
  return $ ExtFunDeclExpr decl 

call :: Parser Expression
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ CallExpr name args

anyExpr :: Parser Expression
anyExpr = try floating
      <|> try int
      <|> try boolean
      <|> try value
      <|> try function
      <|> try extFunction
      <|> try call
      <|> variable
      <|> parens expr

expr :: Parser Expression
expr = E.buildExpressionParser table anyExpr

returnStmt :: Parser Statement
returnStmt = do
    reserved "ret" 
    e <- expr
    return $ ReturnStmt e

expressionStmt :: Parser Statement
expressionStmt = do
    e <- value <|> call
    return $ ExpressionStmt e

blockStmt :: Parser Statement
blockStmt = braces $ M.liftM BlockStmt $ many statement

statement :: Parser Statement
statement = try returnStmt
        <|> try expressionStmt
        <|> try blockStmt

contents :: Parser a -> Parser a
contents p = do
  T.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expression]
toplevel = many (function <|> extFunction)

parseExpr :: String -> Either ParseError Expression
parseExpr = parse (contents expr) "<stdin>"

parseModule :: String -> Either ParseError [Expression]
parseModule = parse (contents toplevel) "<stdin>"

