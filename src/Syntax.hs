module Syntax where

type Name = String
type TypeName = String

data Operation 
    = Add | Sub | Mul | Div          -- basic arithmetic
    | BitAnd | BitOr                 -- bitwise operations
    | LogNot                         -- logical not
    | Eq | Neq | Lt | Lte | Gt | Gte -- comparison
    deriving (Eq, Ord, Show)

data ValueKind = Immutable | Mutable deriving (Eq, Ord, Show)

data FunctionArgument
    = FunArg Name TypeName deriving (Eq, Ord, Show)
data FunctionDeclaration
    = FunDecl Name [FunctionArgument] TypeName deriving (Eq, Ord, Show)
data ValueDeclaration
    = ValDecl ValueKind Name TypeName Expression deriving (Eq, Ord, Show)

data Expression
    = BooleanExpr Bool
    | IntegerExpr Int
    | FloatExpr Double
    | PrefixOpExpr Operation Expression
    | BinOpExpr Operation Expression Expression
    | VarExpr Name
    | ValDeclExpr ValueDeclaration
    | FunDeclExpr FunctionDeclaration Statement
    | ExtFunDeclExpr FunctionDeclaration 
    | CallExpr Name [Expression]
    deriving (Eq, Ord, Show)

data Statement
    = ReturnStmt Expression
    | ExpressionStmt Expression
    | BlockStmt [Statement]
    | IfStmt Expression Statement
    | AssignmentStmt Name Expression
    deriving (Eq, Ord, Show)
