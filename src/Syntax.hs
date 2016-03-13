module Syntax ( Name
              , Operation(..)
              , BindingKind(..)
              , BitCount
              , TupleFiled(Field)
              , Type(..)
              , isArray, isPointer, isArrayPointer, isFunction
              , ValueBinding(..), FunctionDeclaration(..)
              , funDeclToType, nameOfFunDecl
              , Expression(..), tagOfExpr
              , Statement(..)
              ) where

import qualified Data.List as List
import qualified Data.Maybe as Maybe

type Name = String

data Operation 
    = Add | Sub | Mul | Div          -- basic arithmetic
    | BitAnd | BitOr                 -- bitwise operations
    | LogNot                         -- logical not
    | Eq | Neq | Lt | Lte | Gt | Gte -- comparison
    | ArrayLen                       -- array lenght
    | ValRef | PtrDeRef              -- value reference and pointer drreference
    | MemberOf | DeRefMemberOf       -- . and ->
    deriving (Eq, Ord)

-- TODO: keep this somehow in sync with similar table Parser
instance Show Operation where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show BitAnd = "&"
    show BitOr = "|"
    show LogNot = "not"
    show Eq = "=="
    show Neq = "/="
    show Lt = "<"
    show Lte = "<="
    show Gt = ">"
    show Gte = ">="
    show ArrayLen = "#"
    show ValRef = "@"
    show PtrDeRef = "$"
    show MemberOf = "."
    show DeRefMemberOf = "->"

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
          | TypeBottom -- used by type checker for typing errors
          deriving (Eq, Ord)

printFloatingType n
    | n == 64   = "double_t"
    | n == 32   = "float_t"
    | otherwise = "float" ++ show n ++ "_t"

printIntegerType n
    | n == 64   = "int_t"
    | n == 8    = "byte_t"
    | otherwise = "int" ++ show n ++ "_t"

showCommaSep :: Show a => [a] -> String
showCommaSep = List.intercalate ", " . map show

instance Show Type where
    show (TypeFloating n) = printFloatingType n
    show (TypeInteger n) = printIntegerType n
    show TypeBoolean = "bool_t"
    show TypeUnit = "unit_t"
    show (TypeArray t) = "[" ++ show t ++ "]"
    show (TypePointer t) = "^" ++ show t
    show (TypeUnknow name) = name ++ "?"
    show (TypeFunction args ret)
        = "(" ++ showCommaSep args ++ ") -> " ++ show ret
    show (TypeTuple name fields)
        = if name /= "" then name else "(" ++ fieldsStr ++ ")"
      where
        fieldsStr = showCommaSep $ map (\(Field _ t) -> t) fields
    show TypeBottom = "_bottom_"

isArray :: Type -> Bool
isArray TypeArray{} = True
isArray _ = False

isPointer :: Type -> Bool
isPointer TypePointer{} = True
isPointer _ = False

isArrayPointer :: Type -> Bool
isArrayPointer (TypePointer ty) = isArray ty
isArrayPointer _ = False

isFunction :: Type -> Bool
isFunction TypeFunction{} = True
isFunction _ = False

data ValueBinding = ValBind BindingKind Name Type deriving (Eq, Ord)

showBindingKind :: BindingKind -> String
showBindingKind Immutable = ""
showBindingKind Mutable = "!"

instance Show ValueBinding where
    show (ValBind k n t) = showBindingKind k ++ n ++ " " ++ show t 

data FunctionDeclaration
    = FunDecl Name [ValueBinding] Type deriving (Eq, Ord, Show)

funDeclToType :: FunctionDeclaration -> Type
funDeclToType (FunDecl _ args retType)
    = TypeFunction (map (\(ValBind _ _ ty) -> ty) args) retType

nameOfFunDecl :: FunctionDeclaration -> String
nameOfFunDecl (FunDecl name _ _) = name

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
    deriving (Eq, Ord)

isCharLiteral :: Expression a -> Bool
isCharLiteral CharacterExpr{} = True
isCharLiteral _ = False

isStringLiteral :: Expression a -> Bool
isStringLiteral (ArrayExpr es _) = all isCharLiteral es
isStringLiteral _ = False

getLiteralChar :: Expression a -> Maybe Char
getLiteralChar (CharacterExpr c _) = Just c
getLiteralChar _ = Nothing

getLiteralString :: Expression a -> String
getLiteralString (ArrayExpr es _) = Maybe.mapMaybe getLiteralChar es
getLiteralString _ = ""

instance Show (Expression a) where
    show (BooleanExpr b _) = if b then "true" else "false"
    show (IntegerExpr i _) = show i
    show (CharacterExpr c _) = show c
    show (FloatExpr d _) = show d
    show e@(ArrayExpr es _) 
        | isStringLiteral e = "\"" ++ getLiteralString e ++ "\""
        | otherwise = "[" ++ showCommaSep es ++ "]" 
    show (AnonTupleExpr es _) = "(" ++ showCommaSep es ++ ")" 
    show (PrefixOpExpr op e _) =  show op ++ show e
    show (BinOpExpr op e1 e2 _) = show e1 ++ " " ++  show op ++ " " ++ show e2
    show (ElementOfExpr n e _) = n ++ "[" ++ show e ++ "]"
    show (VarExpr n _) = n
    show (ValDeclExpr b e _) = "val " ++ show b ++ " = " ++ show e
    show (ValDestructuringExpr bs e _) = "val (" ++ showCommaSep bs ++ ") = " ++ show e
    show (FunDeclExpr decl stmt _) = "<TODO: fun decl>"
    show (NamedTupleDeclExpr n bs _) = "<TODO: tuple decl>"
    show (CallExpr n es _) = n ++ "(" ++ showCommaSep es ++ ")"
    show (CastExpr t e _) = show t ++ "(" ++ show e ++ ")"

    show expr = "<failed to print expression>"

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
    deriving (Eq, Ord)

instance Show (Statement a) where
    show (ReturnStmt e) = "return " ++ show e
    show (ExpressionStmt e) = show e
    show (BlockStmt stmts) = "{" ++ List.intercalate "\n" (map show stmts) ++ "}"
    show (IfStmt cond body) = "if " ++ show cond ++ show body
    show (WhileStmt cond body) = "while " ++ show cond ++ show body
    show (AssignmentStmt lhs rhs) = show lhs ++ " = " ++ show rhs

