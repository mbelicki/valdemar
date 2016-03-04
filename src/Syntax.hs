module Syntax where

import Data.List as List

type Name = String

data Operation 
    = Add | Sub | Mul | Div          -- basic arithmetic
    | BitAnd | BitOr                 -- bitwise operations
    | LogNot                         -- logical not
    | Eq | Neq | Lt | Lte | Gt | Gte -- comparison
    | ArrayLen                       -- array lenght
    | ValRef | PtrDeRef              -- value reference and pointer drreference
    | MemberOf | DeRefMemberOf       -- . and ->
    deriving (Eq, Ord, Show)

data BindingKind = Immutable | Mutable deriving (Eq, Ord, Show)

type BitCount = Int

data TupleFiled = Field Name Type deriving (Eq, Ord, Show)

data Type = TypeFloating BitCount
          | TypeInteger BitCount
          | TypeBoolean
          | TypeUnit
          | TypeArray Type
          | TypePointer Type
          | TypeFunction [Type] Type
          | TypeTuple Name [TupleFiled]
          | TypeUnknow Name
          deriving (Eq, Ord)

printFloatingType n
    | n == 64   = "double_t"
    | n == 32   = "float_t"
    | otherwise = "float" ++ show n ++ "_t"

instance Show Type where
    show (TypeFloating n) = printFloatingType n
    show (TypeInteger n) = "int" ++ show n ++ "_t"
    show TypeBoolean = "bool_t"
    show TypeUnit = "unit_t"
    show (TypeArray t) = "[" ++ show t ++ "]"
    show (TypePointer t) = "^" ++ show t
    show (TypeUnknow name) = name ++ "?"
    show (TypeFunction args ret)
        = "(" ++ argList ++ ") -> " ++ show ret
      where
        argList = List.intercalate ", " $ map show args

    show (TypeTuple name fields)
        = if name /= "" then name
          else "(" ++ fieldList ++ ")"
      where
        fieldList = List.intercalate ", " $ map (\(Field _ t) -> show t) fields

isArray :: Type -> Bool
isArray (TypeArray _) = True
isArray _ = False

isPointer :: Type -> Bool
isPointer (TypePointer _) = True
isPointer _ = False

isArrayPointer :: Type -> Bool
isArrayPointer (TypePointer ty) = isArray ty
isArrayPointer _ = False

data ValueBinding
    = ValBind BindingKind Name Type deriving (Eq, Ord, Show)
data FunctionDeclaration
    = FunDecl Name [ValueBinding] Type deriving (Eq, Ord, Show)

funDeclToType :: FunctionDeclaration -> Type
funDeclToType (FunDecl _ args retType)
    = TypeFunction (map (\(ValBind _ _ ty) -> ty) args) retType

data Expression tag
    = BooleanExpr Bool tag
    | IntegerExpr Int tag
    | CharacterExpr Char tag
    | FloatExpr Double tag
    | ArrayExpr [Expression tag] tag
    | AnonTupleExpr [Expression tag] tag
    | PrefixOpExpr Operation (Expression tag) tag
    | BinOpExpr Operation (Expression tag) (Expression tag) tag
    | ElementOfExpr Name (Expression tag) tag
    | VarExpr Name tag
    | ValDeclExpr ValueBinding (Expression tag) tag
    | ValDestructuringExpr [ValueBinding] (Expression tag) tag
    | FunDeclExpr FunctionDeclaration (Statement tag) tag
    | ExtFunDeclExpr FunctionDeclaration tag
    | NamedTupleDeclExpr Name [ValueBinding] tag
    | CallExpr Name [Expression tag] tag
    | CastExpr Type (Expression tag) tag
    deriving (Eq, Ord, Show)

tagOfExpr :: Expression a -> a
tagOfExpr (BooleanExpr _ tag) = tag
tagOfExpr (IntegerExpr _ tag) = tag
tagOfExpr (FloatExpr _ tag) = tag
tagOfExpr (ArrayExpr _ tag) = tag
tagOfExpr (AnonTupleExpr _ tag) = tag
tagOfExpr (PrefixOpExpr _ _ tag) = tag
tagOfExpr (BinOpExpr _ _ _ tag) = tag
tagOfExpr (ElementOfExpr _ _ tag) = tag
tagOfExpr (VarExpr _ tag) = tag
tagOfExpr (ValDeclExpr _ _ tag) = tag
tagOfExpr (ValDestructuringExpr _ _ tag) = tag
tagOfExpr (FunDeclExpr _ _ tag) = tag
tagOfExpr (ExtFunDeclExpr _ tag) = tag
tagOfExpr (CallExpr _ _ tag) = tag
tagOfExpr (CastExpr _ _ tag) = tag

data Statement a
    = ReturnStmt (Expression a)
    | ExpressionStmt (Expression a)
    | BlockStmt [Statement a]
    | IfStmt (Expression a) (Statement a)
    | WhileStmt (Expression a) (Statement a)
    | AssignmentStmt (Expression a) (Expression a)
    deriving (Eq, Ord, Show)

