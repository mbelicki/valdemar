module Syntax where

type Name = String
type TypeName = String

data Operation = Add | Sub | Mul | Div deriving (Eq, Ord, Show)

data FunArg = FunArg Name TypeName deriving (Eq, Ord, Show)

data Expr
    = Integer Int
    | Float Double
    | BinOp Operation Expr Expr
    | Var Name
    | ValDecl Name TypeName Expr
    | FunDecl Name [FunArg] TypeName [Statement]
    | ExtFunDecl Name [FunArg] TypeName
    | Call Name [Expr]
    deriving (Eq, Ord, Show)

data Statement
    = Return Expr
    | DeclStatement Expr
    | CallStatement Expr
    deriving (Eq, Ord, Show)
