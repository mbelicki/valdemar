{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TypeChecker
    ( typeCheck
    ) where

import qualified Syntax as S

import Control.Monad as M
import Control.Monad.State
import Control.Applicative

import Data.Maybe as Maybe
import Data.List as List

-- udnderlying data types and state setup

type Decl = (S.Name, S.Type)

data Scope 
    = Scope 
        { scopeDeclarations :: [Decl] 
        , scopeParent :: Maybe Scope
        }

data CheckerState
    = CheckerState
        { checkerGlobalScope :: Scope
        , checkerLocalScope :: Scope
        }

newtype TypeChecker a = TypeChecker { runTypeChecker :: State CheckerState a }
  deriving (Functor, Applicative, Monad, MonadState CheckerState)

emptyScope :: Scope
emptyScope = Scope [] Nothing

emptyChecker :: CheckerState
emptyChecker = CheckerState emptyScope emptyScope

executeChecker :: TypeChecker a -> a
executeChecker tc = evalState (runTypeChecker tc) emptyChecker

-- scope operations:

appendDecl :: Decl -> Scope -> Scope
appendDecl d s = s { scopeDeclarations = decls } where decls = d : scopeDeclarations s

findDeclInScope :: Scope -> S.Name -> Maybe Decl
findDeclInScope scope name
    = if Maybe.isJust maybeDecl then maybeDecl else maybeParentDecl
        where
            maybeParent = scopeParent scope
            decls = scopeDeclarations scope
            maybeDecl = find (\(n, _) -> n == name) decls

            maybeParentDecl = maybeParent >>= (`findDeclInScope` name)

-- cheker state operations:

getGlobals :: TypeChecker Scope
getGlobals = gets checkerGlobalScope

getLocals :: TypeChecker Scope
getLocals = gets checkerLocalScope

addGlobalDecl :: Decl -> TypeChecker ()
addGlobalDecl d = do
    globals <- M.liftM (appendDecl d) getGlobals 
    modify $ \s -> s { checkerGlobalScope = globals }

addLocalDecl :: Decl -> TypeChecker ()
addLocalDecl d = do
    locals <- M.liftM (appendDecl d) getLocals
    modify $ \s -> s { checkerLocalScope = locals }

pushScope :: TypeChecker ()
pushScope = do
    locals <- getLocals
    let newLocals = emptyScope { scopeParent = Just locals }
    modify $ \s -> s { checkerLocalScope = newLocals }

popScope :: TypeChecker (Maybe Scope)
popScope = do
    maybeParent <- M.liftM scopeParent getLocals
    case maybeParent of
        Nothing    -> modify $ \s -> s { checkerLocalScope = emptyScope }
        Just scope -> modify $ \s -> s { checkerLocalScope = scope }
    return maybeParent

findDecl :: S.Name -> TypeChecker (Maybe Decl)
findDecl name = do
    localScope  <- getLocals
    globalScope <- getGlobals
    let maybeLocal  = findDeclInScope localScope  name
        maybeGlobal = findDeclInScope globalScope name
    return $ if Maybe.isJust maybeLocal then maybeLocal else maybeGlobal

assertType :: S.Type -> S.Type -> String -> TypeChecker ()
assertType expected actual message
    = when (expected /= actual) $ error $ message ++ explanation
        where
            explanation = " expected: " ++ show expected ++ " found: " ++ show actual

-- entry point:

typeCheck :: [S.Expression ()] -> [S.Expression S.Type]
typeCheck = executeChecker . checkTree

-- main checking method
checkTree :: [S.Expression ()] -> TypeChecker [S.Expression S.Type]
checkTree ast = do
    -- find and set all globals
    modify $ \s -> s { checkerGlobalScope = findGlobals ast }
    -- for body of each function do the typing
    M.forM ast transformExpression 

transformExpression :: S.Expression () -> TypeChecker (S.Expression S.Type)
-- trivially typable expresions:
transformExpression (S.BooleanExpr v _) = return $ S.BooleanExpr v S.TypeBoolean
transformExpression (S.IntegerExpr v _) = return $ S.IntegerExpr v $ S.TypeInteger 64
transformExpression (S.FloatExpr   v _) = return $ S.FloatExpr v $ S.TypeFloating 64
-- array literal:
transformExpression (S.ArrayExpr rawItems _) = do
    items <- M.forM rawItems transformExpression
    let types     = map S.tagOfExpr items
        arrayType = inferArrayType types
    return $ S.ArrayExpr items arrayType
  where
    checkItemType :: S.Type -> Maybe S.Type -> Maybe S.Type
    checkItemType ta (Just tb) = if ta == tb then Just ta else Nothing
    checkItemType _ Nothing = Nothing

    inferArrayType :: [S.Type] -> S.Type
    inferArrayType types
        = Maybe.fromMaybe
            (error "Mismatched types in literal array declaration.") 
            (foldr checkItemType (Maybe.listToMaybe types) types)

-- operators:
transformExpression (S.PrefixOpExpr op arg _) = do
    typed <- transformExpression arg
    return $ S.PrefixOpExpr op typed $ S.tagOfExpr typed

transformExpression (S.BinOpExpr op argA argB _) = do
    typedA <- transformExpression argA
    typedB <- transformExpression argB
    let opType = if S.tagOfExpr typedA == S.tagOfExpr typedB
                 then S.tagOfExpr typedA
                 else error "Mismatched types in operator expression."
    return $ S.BinOpExpr op typedA typedB opType

-- based on defined symbols:
transformExpression (S.VarExpr name _) = do
    let fail = error $ "Unknown variable: " ++ name
    decl <- M.liftM (Maybe.fromMaybe fail) $ findDecl name
    return $ S.VarExpr name $ snd decl

transformExpression (S.CallExpr name args _) = do
    let declFail = error $ "Unknown function: " ++ name
        argTypeFail = error $ "Mismatched argument types in call of function: " ++ name

    decl <- M.liftM (Maybe.fromMaybe declFail) $ findDecl name
    typedArgs <- M.forM args transformExpression
    
    let actualArgTypes   = map S.tagOfExpr typedArgs
        expectedArgTypes = getArgTypes $ snd decl

        typedExpr = S.CallExpr name typedArgs $ getReturnType $ snd decl

    return $ if actualArgTypes == expectedArgTypes
             then typedExpr
             else argTypeFail
    where
        getReturnType :: S.Type -> S.Type
        getReturnType (S.TypeFunction _ ret) = ret

        getArgTypes :: S.Type -> [S.Type]
        getArgTypes (S.TypeFunction args _) = args

transformExpression (S.ElementOfExpr name index _) = do
    typedIndex <- transformExpression index
    let indexType = S.tagOfExpr typedIndex
    -- check if index has correct type
    assertType indexType (S.TypeInteger 64) "Invalid index type"

    let declFail = error $ "Unknown array: " ++ name
    arrayDecl <- M.liftM (Maybe.fromMaybe declFail) $ findDecl name
    -- check if array declaration from current scope has correct type
    M.unless (S.isArrayPointer $ snd arrayDecl) $ error $ name ++ " is not array pointer."

    return $ S.ElementOfExpr name typedIndex (innerType $ snd arrayDecl)
    where
        innerType :: S.Type -> S.Type
        innerType (S.TypeArray t) = t

transformExpression (S.ValDeclExpr (S.ValDecl kind name ty rawValue) _) = do
    typedValue <- transformExpression rawValue
    
    let message = "In binding of " ++ name
    assertType (S.tagOfExpr typedValue) ty message

    addLocalDecl (name, ty)

    return $ S.ValDeclExpr (S.ValDecl kind name ty typedValue) ty

-- function declarations:
transformExpression (S.FunDeclExpr funDecl stmt _) = do
    typedStmt <- transformStatement stmt
    return $ S.FunDeclExpr funDecl typedStmt $ S.funDeclToType funDecl

transformExpression (S.ExtFunDeclExpr funDecl _)
    = return $ S.ExtFunDeclExpr funDecl $ S.funDeclToType funDecl

transformStatement :: S.Statement () -> TypeChecker (S.Statement S.Type)
transformStatement (S.ReturnStmt rawExpr) = do
    typedExpr <- transformExpression rawExpr
    return $ S.ReturnStmt typedExpr

transformStatement (S.ExpressionStmt rawExpr) = do
    typedExpr <- transformExpression rawExpr
    return $ S.ExpressionStmt typedExpr
    
transformStatement (S.BlockStmt rawStmts) = do
    typedStmts <- M.forM rawStmts transformStatement
    return $ S.BlockStmt typedStmts

transformStatement (S.IfStmt rawCondition rawBody) = do
    typedCondition <- transformExpression rawCondition
    assertType S.TypeBoolean (S.tagOfExpr typedCondition) "If condtition expression:"

    typedBody <- transformStatement rawBody
    return $ S.IfStmt typedCondition typedBody

transformStatement (S.AssignmentStmt name rawExpr) = do
    typedExpr <- transformExpression rawExpr
    
    let fail = error $ "Unknown variable: " ++ name
    decl <- M.liftM (Maybe.fromMaybe fail) $ findDecl name
    
    assertType (snd decl) (S.tagOfExpr typedExpr) $ "Cannot assing to " ++ name

    return $ S.AssignmentStmt name typedExpr
    

findGlobals :: [S.Expression a] -> Scope
findGlobals = buildScope . map funcToDecl
    where
        getDecl :: S.FunctionDeclaration -> Decl
        getDecl decl@(S.FunDecl name _ _) = (name, S.funDeclToType decl)

        funcToDecl :: S.Expression a -> Decl
        funcToDecl (S.FunDeclExpr d _ _) = getDecl d
        funcToDecl (S.ExtFunDeclExpr d _ ) = getDecl d

        buildScope :: [Decl] -> Scope
        buildScope decls = emptyScope { scopeDeclarations = decls }

