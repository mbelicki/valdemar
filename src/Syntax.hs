module Syntax where

type Name = String

data Operation 
    = Add | Sub | Mul | Div          -- basic arithmetic
    | BitAnd | BitOr                 -- bitwise operations
    | LogNot                         -- logical not
    | Eq | Neq | Lt | Lte | Gt | Gte -- comparison
    deriving (Eq, Ord, Show)

data ValueKind = Immutable | Mutable deriving (Eq, Ord, Show)

type BitCount = Int

data Type = TypeFloating BitCount
          | TypeInteger BitCount
          | TypeBoolean
          | TypeUnit
          | TypeArray Type
          | TypePointer Type
          | TypeFunction [Type] Type
          deriving (Eq, Ord, Show)

data FunctionArgument
    = FunArg Name Type deriving (Eq, Ord, Show)
data FunctionDeclaration
    = FunDecl Name [FunctionArgument] Type deriving (Eq, Ord, Show)
data ValueDeclaration a
    = ValDecl ValueKind Name Type (Expression a) deriving (Eq, Ord, Show)

data Expression tag
    = BooleanExpr Bool tag
    | IntegerExpr Int tag
    | FloatExpr Double tag
    | ArrayExpr [Expression tag] tag
    | PrefixOpExpr Operation (Expression tag) tag
    | BinOpExpr Operation (Expression tag) (Expression tag) tag
    | ElementOfExpr Name (Expression tag) tag
    | VarExpr Name tag
    | ValDeclExpr (ValueDeclaration tag) tag
    | FunDeclExpr FunctionDeclaration (Statement tag) tag
    | ExtFunDeclExpr FunctionDeclaration  tag
    | CallExpr Name [Expression tag] tag
    deriving (Eq, Ord, Show)

data Statement a
    = ReturnStmt (Expression a)
    | ExpressionStmt (Expression a)
    | BlockStmt [Statement a]
    | IfStmt (Expression a) (Statement a)
    | AssignmentStmt Name (Expression a)
    deriving (Eq, Ord, Show)
