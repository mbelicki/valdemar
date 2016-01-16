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
type TypeAlias = (S.Name, S.Type)

data Scope 
    = Scope 
        { scopeDeclarations :: [Decl] 
        , scopeAliases :: [TypeAlias]
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
emptyScope = Scope [] [] Nothing

emptyChecker :: CheckerState
emptyChecker = CheckerState emptyScope emptyScope

executeChecker :: TypeChecker a -> a
executeChecker tc = evalState (runTypeChecker tc) emptyChecker

-- scope operations:

appendDecl :: Decl -> Scope -> Scope
appendDecl d s = s { scopeDeclarations = decls } where decls = d : scopeDeclarations s

appendAlias :: TypeAlias -> Scope -> Scope
appendAlias a s = s { scopeAliases = aliases } where aliases = a : scopeAliases s

findDeclInScope :: Scope -> S.Name -> Maybe Decl
findDeclInScope = findInScope scopeDeclarations

findAliasInScope :: Scope -> S.Name -> Maybe TypeAlias
findAliasInScope = findInScope scopeAliases

findInScope :: (Scope -> [(S.Name, a)]) -> Scope -> S.Name -> Maybe (S.Name, a)
findInScope getter scope name
    = if Maybe.isJust maybeDecl then maybeDecl else maybeParentDecl
        where
            maybeParent = scopeParent scope
            decls = getter scope
            maybeDecl = find (\(n, _) -> n == name) decls

            maybeParentDecl = maybeParent >>= (\s -> findInScope getter s name)

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
assertType (S.TypeTuple _ exp_tys) (S.TypeTuple _ act_tys) message
    = when (exp_tys /= act_tys) $ error message
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

findGlobals :: [S.Expression a] -> Scope
findGlobals s = buildScope decls aliases
    where
        getDecl :: S.FunctionDeclaration -> Decl
        getDecl decl@(S.FunDecl name _ _) = (name, S.funDeclToType decl)

        funcToDecl :: S.Expression a -> Maybe Decl
        funcToDecl (S.FunDeclExpr d _ _) = Just $ getDecl d
        funcToDecl (S.ExtFunDeclExpr d _ ) = Just $ getDecl d
        funcToDecl _ = Nothing

        fieldsToTypes :: [S.ValueBinding] -> [S.Type]
        fieldsToTypes = map (\(S.ValBind _ _ t) -> t)

        tupleToAlias :: S.Expression a -> Maybe TypeAlias
        tupleToAlias (S.NamedTupleDeclExpr name fileds _)
            = Just (name, S.TypeTuple name $ fieldsToTypes fileds)
        tupleToAlias _ = Nothing

        decls = mapMaybe funcToDecl s
        aliases = mapMaybe tupleToAlias s

        buildScope :: [Decl] -> [TypeAlias] -> Scope
        buildScope decls aliases
            = emptyScope { scopeDeclarations = decls, scopeAliases = aliases }

-- tranformations:

resolveType :: S.Type -> TypeChecker S.Type
resolveType t@(S.TypeUnknow name) = do 
    globalScope <- getGlobals
    let maybeAlias = findAliasInScope globalScope name
    if Maybe.isNothing maybeAlias
        then return t
        else do
            let Just (nm, ty) = maybeAlias
            return ty

resolveType t@(S.TypeArray ty) = do
    resTy <- resolveType ty
    return $ S.TypeArray resTy

resolveType t@(S.TypePointer ty) = do
    resTy <- resolveType ty
    return $ S.TypePointer resTy

resolveType t@(S.TypeFunction args ret) = do
    resArgs <- M.mapM resolveType args
    resRet <- resolveType ret
    return $ S.TypeFunction resArgs resRet

resolveType a = return a


resolveBinding :: S.ValueBinding -> TypeChecker S.ValueBinding
resolveBinding (S.ValBind kind nm ty) = do
    resTy <- resolveType ty
    return $ S.ValBind kind nm resTy


transformExpression :: S.Expression () -> TypeChecker (S.Expression S.Type)
-- trivially typable expresions:
transformExpression (S.BooleanExpr   v _) = return $ S.BooleanExpr v S.TypeBoolean
transformExpression (S.IntegerExpr   v _) = return $ S.IntegerExpr v $ S.TypeInteger 64
transformExpression (S.CharacterExpr v _) = return $ S.IntegerExpr (fromEnum v) $ S.TypeInteger 8
transformExpression (S.FloatExpr     v _) = return $ S.FloatExpr v $ S.TypeFloating 64
transformExpression (S.CastExpr    t v _) = do 
    typedExpr <- transformExpression v
    return $ S.CastExpr t typedExpr t

-- array literal:
transformExpression (S.ArrayExpr rawItems _) = do
    items <- M.forM rawItems transformExpression
    let types         = map S.tagOfExpr items
        arrayBaseType = inferArrayType types
        arrayType     = S.TypePointer $ S.TypeArray arrayBaseType
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

-- tuple literal:
transformExpression (S.AnonTupleExpr rawItems _) = do
    items <- M.forM rawItems transformExpression
    let types     = map S.tagOfExpr items
        tupleType = S.TypeTuple "" types
    return $ S.AnonTupleExpr items tupleType

-- operators:
transformExpression (S.PrefixOpExpr op arg _) = do
    typed <- transformExpression arg
    return $ S.PrefixOpExpr op typed $ getOpType op $ S.tagOfExpr typed
  where
    getOpType :: S.Operation -> S.Type -> S.Type
    getOpType op _ | op `elem` [S.ArrayLen] = S.TypeInteger 64
    getOpType _ argType = argType

transformExpression (S.BinOpExpr op argA argB _) = do
    typedA <- transformExpression argA
    typedB <- transformExpression argB
    let opType = if S.tagOfExpr typedA == S.tagOfExpr typedB
                 then getOpType op (S.tagOfExpr typedA)
                 else error "Mismatched types in operator expression."
    return $ S.BinOpExpr op typedA typedB opType
  where
    getOpType :: S.Operation -> S.Type -> S.Type
    getOpType op _ | op `elem` [S.Eq, S.Neq, S.Lt, S.Lte, S.Gt, S.Gte, S.LogNot] = S.TypeBoolean
    getOpType _ argType = argType

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
    assertType (S.TypeInteger 64) indexType "Invalid index type"

    let declFail = error $ "Unknown array: " ++ name
    arrayDecl <- M.liftM (Maybe.fromMaybe declFail) $ findDecl name
    -- check if array declaration from current scope has correct type
    M.unless (S.isArrayPointer $ snd arrayDecl) $ error $ name ++ " is not array pointer."

    return $ S.ElementOfExpr name typedIndex (innerType $ snd arrayDecl)
    where
        innerType :: S.Type -> S.Type
        innerType (S.TypePointer (S.TypeArray t)) = t

transformExpression (S.ValDeclExpr (S.ValBind kind name t) rawValue _) = do
    typedValue <- transformExpression rawValue
    ty <- resolveType t
    otherTy <- resolveType $ S.tagOfExpr typedValue
    
    let message = "In binding of '" ++ name ++ "'"
    assertType ty otherTy message

    addLocalDecl (name, ty)

    return $ S.ValDeclExpr (S.ValBind kind name ty) typedValue ty

transformExpression (S.ValDestructuringExpr bindings rawValue _) = do
    typedValue <- transformExpression rawValue
    otherTy <- resolveType $ S.tagOfExpr typedValue

    resolvedBindings <- M.mapM resolveBinding bindings
    let packedType = S.TypeTuple "" (map (\(S.ValBind _ _ t) -> t) resolvedBindings)
    
    let names = foldr (\a b -> b ++ show a) "" resolvedBindings
        message = "In destructuring binding of (" ++ names ++ ")"
    assertType packedType otherTy message

    M.forM_ resolvedBindings $ \(S.ValBind _ name ty) -> addLocalDecl (name, ty)

    return $ S.ValDestructuringExpr resolvedBindings typedValue otherTy

-- function declarations:
transformExpression (S.FunDeclExpr funDecl stmt _) = do
    pushScope
    
    declareArguments funDecl
    typedStmt <- transformStatement stmt
    
    popScope

    ty <- resolveType $ S.funDeclToType funDecl
    decl <- resolveFunDecl funDecl

    return $ S.FunDeclExpr decl typedStmt ty
  where
    declareArguments :: S.FunctionDeclaration -> TypeChecker ()
    declareArguments (S.FunDecl _ args _)
        = M.forM_ args $ \(S.ValBind _ name ty) -> addLocalDecl (name, ty)

    resolveFunDecl :: S.FunctionDeclaration -> TypeChecker S.FunctionDeclaration
    resolveFunDecl (S.FunDecl name args retTy) = do
        resolvedArgs <- M.mapM resolveBinding args
        resolvedRetTy <- resolveType retTy
        return $ S.FunDecl name resolvedArgs resolvedRetTy


transformExpression (S.ExtFunDeclExpr funDecl _)
    = return $ S.ExtFunDeclExpr funDecl $ S.funDeclToType funDecl

transformExpression (S.NamedTupleDeclExpr name fields _)
    = return $ S.NamedTupleDeclExpr name fields S.TypeUnit

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

transformStatement (S.WhileStmt rawCondition rawBody) = do
    typedCondition <- transformExpression rawCondition
    assertType S.TypeBoolean (S.tagOfExpr typedCondition) "While condtition expression:"

    typedBody <- transformStatement rawBody
    return $ S.WhileStmt typedCondition typedBody

transformStatement (S.AssignmentStmt (S.VarExpr name _) rawExpr) = do
    typedExpr <- transformExpression rawExpr
    
    let fail = error $ "Unknown variable: " ++ name
    decl <- M.liftM (Maybe.fromMaybe fail) $ findDecl name
    
    assertType (snd decl) (S.tagOfExpr typedExpr) $ "Cannot assing to " ++ name

    return $ S.AssignmentStmt (S.VarExpr name (snd decl)) typedExpr

transformStatement (S.AssignmentStmt (S.ElementOfExpr name index _) rawExpr) = do
    typedExpr <- transformExpression rawExpr
    typedIndex <- transformExpression index
    
    let fail = error $ "Unknown variable: " ++ name
    decl <- M.liftM (Maybe.fromMaybe fail) $ findDecl name
    
    assertType (getElemType $ snd decl) (S.tagOfExpr typedExpr) $ "Cannot assing to " ++ name
    assertType (S.TypeInteger 64) (S.tagOfExpr typedIndex) "Invalid index type"

    return $ S.AssignmentStmt (S.ElementOfExpr name typedIndex (snd decl)) typedExpr
  where
    getElemType :: S.Type -> S.Type
    getElemType (S.TypePointer (S.TypeArray t)) = t
    getElemType t = t
    

