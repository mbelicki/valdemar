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
          deriving (Eq, Ord, Show)

data FunctionArgument
    = FunArg Name Type deriving (Eq, Ord, Show)
data FunctionDeclaration
    = FunDecl Name [FunctionArgument] Type deriving (Eq, Ord, Show)
data ValueDeclaration
    = ValDecl ValueKind Name Type Expression deriving (Eq, Ord, Show)

data Expression
    = BooleanExpr Bool
    | IntegerExpr Int
    | FloatExpr Double
    | ArrayExpr [Expression]
    | PrefixOpExpr Operation Expression
    | BinOpExpr Operation Expression Expression
    | ElementOfExpr Name Expression
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
