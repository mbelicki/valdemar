module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as E
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Char as C

import qualified Control.Monad as M

import Lexer
import Syntax

prefix symbol func
    = E.Prefix (operator symbol >> return (\a -> PrefixOpExpr func a ()))
binary symbol func
    = E.Infix (operator symbol >> return (\a b -> BinOpExpr func a b ()))

table = [ [ binary "."  MemberOf E.AssocLeft
          , binary "->" DeRefMemberOf E.AssocLeft
          ]
        , [ prefix "#" ArrayLen ]
        , [ prefix "not" LogNot ]
        , [ prefix "@" ValRef ]
        , [ prefix "$" PtrDeRef ]
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

typeBoolean :: Parser Type
typeBoolean = reserved "bool_t" >> return TypeBoolean

typeUnit :: Parser Type
typeUnit = reserved "unit_t" >> return TypeUnit

typeFloating :: Parser Type
typeFloating = (reserved "double_t" >> return (TypeFloating 64))
            <|> (reserved "float_t" >> return (TypeFloating 32))

typeInteger :: Parser Type
typeInteger = (reserved "int_t" >> return (TypeInteger 64))
          <|> (reserved "byte_t" >> return (TypeInteger 8))

typeArray :: Parser Type
typeArray = M.liftM TypeArray $ brackets typeDecl

typeString :: Parser Type
typeString = reserved "str_t" >> return (TypePointer $ TypeArray $ TypeInteger 8)

typePointer :: Parser Type
typePointer = do
    operator "^"
    ty <- typeDecl
    return $ TypePointer ty

typeAnonTuple :: Parser Type
typeAnonTuple = M.liftM (TypeTuple "" . map (Field "")) $ parens $ commaSep typeDecl

typeUnknow :: Parser Type
typeUnknow = do
    name <- identifier
    if isValidTypeName name
        then return $ TypeUnknow name
        else unexpected (show name ++ " is not a valid type name.")

typeDecl :: Parser Type
typeDecl = typeArray 
       <|> typePointer
       <|> typeString
       <|> typeInteger 
       <|> typeFloating 
       <|> typeBoolean 
       <|> typeUnit
       <|> typeAnonTuple
       <|> typeUnknow

int :: Parser (Expression ())
int = do
    value <- integer
    return $ IntegerExpr (fromInteger value) ()

sign :: Parser Double
sign =  (char '-' >> return (-1.0))
    <|> (char '+' >> return 1.0)
    <|> return 1.0

floating :: Parser (Expression ())
floating = do 
    s <- sign
    value <- float
    return $ FloatExpr (s * value) ()

boolValue :: Parser Bool
boolValue = (reserved "true"  >> return True)
        <|> (reserved "false" >> return False)

boolean :: Parser (Expression ())
boolean = do
    value <- boolValue
    return $ BooleanExpr value ()

character :: Parser (Expression ())
character = do
    value <- charLiteral
    return $ CharacterExpr value ()

stringValue :: Parser (Expression ())
stringValue = do
    value <- stringLiteral
    let values = map (`CharacterExpr` ()) value
    return $ ArrayExpr values ()

array :: Parser (Expression ())
array = do
    values <- brackets $ commaSep expr
    return $ ArrayExpr values ()

tuple :: Parser (Expression ())
tuple = do
    values <- parens $ commaSep expr
    return $ AnonTupleExpr values ()

variable :: Parser (Expression ())
variable = do
    name <- identifier
    return $ VarExpr name ()

valueDeclKind :: Parser BindingKind
valueDeclKind = (operator "!" >> return Mutable)
            <|> return Immutable

valueDecl :: Parser ValueBinding
valueDecl = do
    kind <- valueDeclKind
    name <- identifier
    typeName <- typeDecl
    return $ ValBind kind name typeName

bindingPack :: Parser [ValueBinding]
bindingPack = do
    names <- commaSepNoDangling kindNamePair
    typeName <- typeDecl
    return $ map (\(kind, name) -> ValBind kind name typeName) names
  where
    kindNamePair :: Parser (BindingKind, Name)
    kindNamePair = do
        kind <- valueDeclKind
        name <- identifier
        return (kind, name)

argList :: Parser [ValueBinding]
argList = do
    let singleArg = M.liftM (: []) valueDecl
    args <- commaSep (try singleArg <|> bindingPack)
    return $ concat args

value :: Parser (Expression ())
value = do
    reserved "val"
    decl <- valueDecl
    reserved "="
    body <- expr
    return $ ValDeclExpr decl body ()

valueDestruct :: Parser (Expression ())
valueDestruct = do
    reserved "val"
    decls <- parens argList
    reserved "="
    body <- expr
    return $ ValDestructuringExpr decls body ()

functionDecl :: Parser FunctionDeclaration
functionDecl = do
    name <- identifier
    args <- parens argList
    reserved "->"
    retType <- typeDecl
    return $ FunDecl name args retType

function :: Parser (Expression ())
function = do
  reserved "fn"
  decl <- functionDecl
  body <- statement
  return $ FunDeclExpr decl body ()

extFunction :: Parser (Expression ())
extFunction = do
  reserved "ext_c"
  decl <- functionDecl
  return $ ExtFunDeclExpr decl ()

namedTuple :: Parser (Expression ())
namedTuple = do
    reserved "tuple"
    name <- identifier
    fields <- braces argList
    return $ NamedTupleDeclExpr name fields ()

cast :: Parser (Expression ())
cast = do
  castType <- typeDecl
  expr <- parens expr
  return $ CastExpr castType expr ()

call :: Parser (Expression ())
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ CallExpr name args ()

elementOf :: Parser (Expression ())
elementOf = do
    array <- identifier
    index <- brackets expr
    return $ ElementOfExpr array index ()

anyExpr :: Parser (Expression ())
anyExpr = try floating
      <|> try int
      <|> try boolean
      <|> try character
      <|> try stringValue
      <|> try array
      <|> try tuple
      <|> try value
      <|> try valueDestruct
      <|> try function
      <|> try extFunction
      <|> try cast
      <|> try call
      <|> try elementOf
      <|> variable
      <|> parens expr

expr :: Parser (Expression ())
expr = E.buildExpressionParser table anyExpr

returnStmt :: Parser (Statement ())
returnStmt = do
    reserved "return"
    e <- expr
    return $ ReturnStmt e

expressionStmt :: Parser (Statement ())
expressionStmt = do
    e <- try value <|> valueDestruct <|> call
    return $ ExpressionStmt e

blockStmt :: Parser (Statement ())
blockStmt = braces $ M.liftM BlockStmt $ many statement

ifStmt :: Parser (Statement ())
ifStmt = do
    reserved "if" 
    cond <- expr
    body <- statement
    return $ IfStmt cond body

whileStmt :: Parser (Statement ())
whileStmt = do
    reserved "while"
    cond <- expr
    body <- statement
    return $ WhileStmt cond body

assignmentStmt :: Parser (Statement ())
assignmentStmt = do
    variable <- expr
    reserved "="
    body <- expr
    return $ AssignmentStmt variable body

statement :: Parser (Statement ())
statement = try returnStmt
        <|> try blockStmt
        <|> try ifStmt
        <|> try whileStmt
        <|> try assignmentStmt
        <|> try expressionStmt

contents :: Parser a -> Parser a
contents p = do
  T.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expression ()]
toplevel = many (function <|> extFunction <|> namedTuple)

parseExpr :: String -> Either ParseError (Expression ())
parseExpr = parse (contents expr) "<stdin>"

parseModule :: String -> Either ParseError [Expression ()]
parseModule = parse (contents toplevel) "<stdin>"

