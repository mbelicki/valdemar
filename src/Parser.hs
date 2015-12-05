module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as E
import qualified Text.Parsec.Token as T

import qualified Control.Monad as M

import Lexer
import Syntax

prefix symbol function = E.Prefix (operator symbol >> return (PrefixOpExpr function))
binary symbol function = E.Infix (operator symbol >> return (BinOpExpr function))

table = [ [ prefix "not" LogNot ]
        , [ binary "*" Mul E.AssocLeft
          , binary "/" Div E.AssocLeft
          ]
        , [ binary "+" Add E.AssocLeft
          , binary "-" Sub E.AssocLeft
          ]
        , [ binary "<"  Lt  E.AssocLeft
          , binary ">"  Gt  E.AssocLeft
          , binary "<=" Lte E.AssocLeft
          , binary ">=" Gte E.AssocLeft
          ]
        , [ binary "==" Eq  E.AssocLeft
          , binary "/=" Neq E.AssocLeft
          ]
        , [ binary "&" BitAnd E.AssocLeft ]
        , [ binary "|" BitOr E.AssocLeft ]
        ]

int :: Parser Expression
int = M.liftM (IntegerExpr . fromInteger) integer


sign :: Parser Double
sign =  (char '-' >> return (-1.0))
    <|> (char '+' >> return 1.0)
    <|> return 1.0

floating :: Parser Expression
floating = do 
    s <- sign
    value <- float
    return $ FloatExpr $ s * value

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

ifStmt :: Parser Statement
ifStmt = do
    reserved "if" 
    cond <- expr
    body <- statement
    return $ IfStmt cond body

statement :: Parser Statement
statement = try returnStmt
        <|> try blockStmt
        <|> try ifStmt
        <|> try expressionStmt

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

