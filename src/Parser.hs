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
typeArray = (M.liftM TypeArray $ brackets typeDecl) <?> "array type declaration"

typeString :: Parser Type
typeString = reserved "str_t" >> return (TypePointer $ TypeArray $ TypeInteger 8)

typePointer :: Parser Type
typePointer = do
    operator "^"
    ty <- typeDecl
    return $ TypePointer ty

typeAnonTuple :: Parser Type
typeAnonTuple
    = (M.liftM (TypeTuple "" . map (Field "")) $ parens $ commaSep typeDecl)
      <?> "anonymous tuple type declaration"

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
       <?> "type declaration"

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
    let values = map (`CharacterExpr` ()) (value ++ "\0")
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
    typeName <- typeDecl <|> return TypeAuto
    return $ ValBind kind name typeName

autoValueDecl :: Parser ValueBinding
autoValueDecl = do
    kind <- valueDeclKind
    name <- identifier
    return $ ValBind kind name TypeAuto

argList :: Parser [ValueBinding]
argList = do
    args <- commaSep valueDecl
    return $ propagateType args
  where
    -- replace _auto_ with type of next variable if there is a next variable
    propagateType [] = []
    propagateType args@(a:as)
        = reverse $ propagateTypeRec (reverse args) (bindingType a)
    
    propagateTypeRec [] _ = []
    propagateTypeRec (a:as) t
        = [ta] ++ propagateTypeRec as (bindingType ta) where ta = setTypeIfAuto a t

    setTypeIfAuto :: ValueBinding -> Type -> ValueBinding
    setTypeIfAuto b@(ValBind k n t) ty = if t == TypeAuto then ValBind k n ty else b

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
    array <- indexableExpr -- TODO: this should be any expression
    index <- brackets expr
    return $ ElementOfExpr array index ()
  where
    indexableExpr :: Parser (Expression ())
    indexableExpr
        = E.buildExpressionParser table
            $ try stringValue <|> try array <|> variable


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
      <?> "expression"

expr :: Parser (Expression ())
expr = E.buildExpressionParser table anyExpr

returnStmt :: Parser (Statement ())
returnStmt = (do
    reserved "return"
    e <- expr
    return $ ReturnStmt e) <?> "return statement"

expressionStmt :: Parser (Statement ())
expressionStmt = do
    e <- try value <|> valueDestruct <|> call
    return $ ExpressionStmt e

blockStmt :: Parser (Statement ())
blockStmt = (braces $ M.liftM BlockStmt $ many statement) <?> "block statement"

ifStmt :: Parser (Statement ())
ifStmt = (do
    reserved "if" 
    cond <- expr
    body <- blockStmt
    return $ IfStmt cond body) <?> "if statement"

whileStmt :: Parser (Statement ())
whileStmt = do
    reserved "while"
    cond <- expr
    update <- updateStmt <|> return Nothing
    body <- blockStmt
    return $ WhileStmt cond update body
  where
    updateStmt = (Just <$> (operator ";" >> try assignmentStmt))

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
        <?> "statement"

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

parseModule :: String -> String -> Either ParseError [Expression ()]
parseModule = parse (contents toplevel)

