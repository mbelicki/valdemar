module Syntax where

type Name = String
type TypeName = String

data Operation 
    = Add | Sub | Mul | Div -- basic arithmetic
    | And | Or              -- boolean operations
    deriving (Eq, Ord, Show)

data FunctionArgument
    = FunArg Name TypeName deriving (Eq, Ord, Show)
data FunctionDeclaration
    = FunDecl Name [FunctionArgument] TypeName deriving (Eq, Ord, Show)
data ValueDeclaration
    = ValDecl Name TypeName Expression deriving (Eq, Ord, Show)

data Expression
    = BooleanExpr Bool
    | IntegerExpr Int
    | FloatExpr Double
    | BinOpExpr Operation Expression Expression
    | VarExpr Name
    | ValDeclExpr ValueDeclaration
    | FunDeclExpr FunctionDeclaration [Statement]
    | ExtFunDeclExpr FunctionDeclaration 
    | CallExpr Name [Expression]
    deriving (Eq, Ord, Show)

data Statement
    = ReturnStmt Expression
    | ExpressionStmt Expression
    deriving (Eq, Ord, Show)
