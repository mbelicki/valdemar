module Syntax where

type Name = String
type TypeName = String

data Operation = Add | Sub | Mul | Div deriving (Eq, Ord, Show)

data Expr
    = Integer Int
    | Float Double
    | BinOp Operation Expr Expr
    | Var Name
    | TypedVar Name TypeName
    | ValDecl Name TypeName Expr
    | FunDecl Name [Expr] TypeName [Statement]
    | ExtFunDecl Name [Expr] TypeName
    | Call Name [Expr]
    deriving (Eq, Ord, Show)

data Statement
    = Return Expr
    | DeclStatement Expr
    | CallStatement Expr
    deriving (Eq, Ord, Show)
