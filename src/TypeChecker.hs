{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TypeChecker
    ( typeCheck
    ) where

import qualified Syntax as S
import qualified Fault as F

import Control.Monad as M
import Control.Monad.State
import Control.Monad.Except
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
        , checkerFaults :: [F.Fault]
        , checkerLastStatement :: Maybe (S.Statement ())
        }

newtype TypeChecker a = TypeChecker { runTypeChecker :: State CheckerState a }
  deriving (Functor, Applicative, Monad, MonadState CheckerState)

emptyScope :: Scope
emptyScope = Scope [] [] Nothing

emptyChecker :: CheckerState
emptyChecker = CheckerState emptyScope emptyScope [] Nothing

executeChecker :: TypeChecker a -> Either [F.Fault] (a, [F.Fault])
executeChecker tc = if hasFailed then Left faults else Right (result, faults)
    where
     (result, finalState) = runState (runTypeChecker tc) emptyChecker
     isError (F.Fault kind _ _) = kind == F.Error
     faults = reverse $ checkerFaults finalState
     hasFailed = any isError faults

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

-- add fault to fault list
addFault :: F.Fault -> TypeChecker ()
addFault f = do
    fs <- gets checkerFaults
    modify $ \s -> s { checkerFaults = f : fs }

-- create and add fault with standard format of context part
createFault :: F.FaultLevel -> String -> String -> TypeChecker ()
createFault level message contextStart = do
    maybeStmt <- getLastStatement
    let stmtPart = maybe "" (\s -> "In statement: '" ++ show s ++ "'") maybeStmt
        context = contextStart ++ "\n" ++ stmtPart
    addFault $ F.Fault level message context


setCurrentStatement :: S.Statement () -> TypeChecker ()
setCurrentStatement stmt = modify $ \s -> s { checkerLastStatement = Just stmt }

getLastStatement :: TypeChecker (Maybe (S.Statement ()))
getLastStatement = gets checkerLastStatement

getGlobals :: TypeChecker Scope
getGlobals = gets checkerGlobalScope

getLocals :: TypeChecker Scope
getLocals = gets checkerLocalScope

addGlobalDecl :: Decl -> TypeChecker ()
addGlobalDecl d = do
    globals <- fmap (appendDecl d) getGlobals 
    modify $ \s -> s { checkerGlobalScope = globals }

addLocalDecl :: Decl -> TypeChecker ()
addLocalDecl d = do
    locals <- fmap (appendDecl d) getLocals
    modify $ \s -> s { checkerLocalScope = locals }

pushScope :: TypeChecker ()
pushScope = do
    locals <- getLocals
    let newLocals = emptyScope { scopeParent = Just locals }
    modify $ \s -> s { checkerLocalScope = newLocals }

popScope :: TypeChecker (Maybe Scope)
popScope = do
    maybeParent <- fmap scopeParent getLocals
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

isTuple :: S.Type -> Bool
isTuple (S.TypeTuple _ _) = True
isTuple _ = False

isPointer :: S.Type -> Bool
isPointer (S.TypePointer _) = True
isPointer _ = False

-- entry point:

typeCheck :: [S.Expression ()] -> F.MaybeAst S.Type
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

        fieldsToTypes :: [S.ValueBinding] -> [S.TupleFiled]
        fieldsToTypes = map (\(S.ValBind _ n t) -> S.Field n t)

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
            let Just (_, ty) = maybeAlias
            resolveType ty -- TODO: infinite recursion on list node tuples

resolveType (S.TypeArray ty) = do
    resTy <- resolveType ty
    return $ S.TypeArray resTy

resolveType (S.TypePointer ty) = do
    resTy <- resolveType ty
    return $ S.TypePointer resTy

resolveType (S.TypeFunction args ret) = do
    resArgs <- M.mapM resolveType args
    resRet <- resolveType ret
    return $ S.TypeFunction resArgs resRet

resolveType a = return a

resolveBinding :: S.ValueBinding -> TypeChecker S.ValueBinding
resolveBinding (S.ValBind kind nm ty) = do
    resTy <- resolveType ty
    return $ S.ValBind kind nm resTy

simplifyType :: S.Type -> S.Type
simplifyType t@(S.TypeTuple name fields)
    | null fields = S.TypeUnit
    | length fields == 1 = (\(S.Field _ t) -> t) $ head fields
    | otherwise = t
simplifyType t = t


needsCast :: S.Type -> S.Type -> Bool
needsCast t1@(S.TypeTuple n1 _) t2@(S.TypeTuple n2 _) = n1 /= n2
needsCast _ S.TypeBottom = False
needsCast actual expected = expected /= actual

hasTheSameLayout :: S.Type -> S.Type -> Bool
hasTheSameLayout  S.TypeBoolean S.TypeBoolean = True
hasTheSameLayout (S.TypeInteger n1) (S.TypeInteger n2) = n1 == n2
hasTheSameLayout (S.TypeFloating n1) (S.TypeFloating n2) = n1 == n2
hasTheSameLayout (S.TypeArray t1) (S.TypeArray t2) = hasTheSameLayout t1 t2
hasTheSameLayout (S.TypePointer t1) (S.TypePointer t2) = True
hasTheSameLayout (S.TypeTuple _ fields1) (S.TypeTuple _ fields2)
    = and $ zipWith hasTheSameLayout fs1 fs2
  where
    stripFiledNames = map (\(S.Field _ t) -> t)
    fs1 = stripFiledNames fields1
    fs2 = stripFiledNames fields1

hasTheSameLayout _ _ = False

canCastImplicitly :: S.Type -> S.Type -> Bool
canCastImplicitly  S.TypeBoolean      (S.TypeInteger _)   = True
canCastImplicitly  S.TypeBoolean      (S.TypeFloating _)  = True
canCastImplicitly (S.TypeInteger _)   (S.TypeFloating _)  = True
canCastImplicitly (S.TypeInteger n1)  (S.TypeInteger n2)  = n1 <= n2
canCastImplicitly (S.TypeFloating n1) (S.TypeFloating n2) = n1 <= n2
canCastImplicitly t1@(S.TypeTuple name1 _) t2@(S.TypeTuple name2 _) 
    = nameCompatible && fieldsCompatible
  where
    nameCompatible = name1 == "" || name2 == "" || name1 == name2
    fieldsCompatible = hasTheSameLayout t1 t2
-- allow to use single filed tuples as values of the filed's type
canCastImplicitly t1@S.TypeTuple{} t2
    -- = hasTheSameLayout t1 (S.TypeTuple "" [S.Field "" t2])
    = canCastImplicitly (simplifyType t1) t2

canCastImplicitly (S.TypePointer t1) (S.TypePointer t2) = hasTheSameLayout t1 t2
canCastImplicitly a b = a == b

castExprImplicitly :: S.Type -> S.Expression S.Type -> TypeChecker (S.Expression S.Type)
castExprImplicitly desiredType typedExpr
     | not castNeeded = return typedExpr
     | castPossible = return $ S.CastExpr desiredType typedExpr desiredType
     | otherwise = do
        createFault F.Error failMsg failCtx
        return typedExpr
  where
    actualType = S.tagOfExpr typedExpr
    castNeeded = needsCast actualType desiredType
    castPossible = canCastImplicitly actualType desiredType

    failMsg = "Cannot implicitly cast: '" ++ show actualType ++ "' to '" 
        ++ show desiredType ++ "'."
    failCtx = "Casted expression: '" ++ show typedExpr ++ "'"

findCommonImplicitTypes :: [S.Type] -> [S.Type]
findCommonImplicitTypes ts = List.nub $ filter (canCastTo ts) ts
  where
    simplified = map simplifyType ts

    canCastTo :: [S.Type] -> S.Type -> Bool
    canCastTo ts t = all (`canCastImplicitly` t) ts


transformExpression :: S.Expression () -> TypeChecker (S.Expression S.Type)
-- trivially typable expresions:
transformExpression (S.BooleanExpr   v _) = return $ S.BooleanExpr v S.TypeBoolean
transformExpression (S.IntegerExpr   v _) = return $ S.IntegerExpr v $ S.TypeInteger 64
transformExpression (S.CharacterExpr v _) = return $ S.IntegerExpr (fromEnum v) $ S.TypeInteger 8
transformExpression (S.FloatExpr     v _) = return $ S.FloatExpr v $ S.TypeFloating 64
transformExpression (S.CastExpr    t v _) = do 
    typedExpr <- transformExpression v
    resolveType <- resolveType t
    return $ S.CastExpr resolveType typedExpr resolveType

-- array literal:
transformExpression e@(S.ArrayExpr rawItems _) = do
    items <- M.forM rawItems transformExpression
    arrayBaseType <- inferArrayType $ map S.tagOfExpr items
    return $ S.ArrayExpr items $ S.TypePointer $ S.TypeArray arrayBaseType
  where
    failMsg = "Mismatched types in literal array declaration."
    failCtx = "In expression: '" ++ show e ++ "'"

    checkItemType :: S.Type -> Maybe S.Type -> Maybe S.Type
    checkItemType ta (Just tb) = if ta == tb then Just ta else Nothing
    checkItemType _ Nothing = Nothing

    inferArrayType :: [S.Type] -> TypeChecker S.Type
    inferArrayType types = do 
        let maybeTypes = foldr checkItemType (Maybe.listToMaybe types) types
        case maybeTypes of
            (Just ts) -> return ts
            Nothing   -> do
                createFault F.Error failMsg failCtx
                return S.TypeBottom

-- tuple literal:
transformExpression (S.AnonTupleExpr rawItems _) = do
    items <- M.forM rawItems transformExpression
    let types     = map (S.Field "" . S.tagOfExpr) items
        tupleType = S.TypeTuple "" types
    return $ if length items == 1 
             then head items
             else S.AnonTupleExpr items tupleType

-- operators:
transformExpression (S.PrefixOpExpr op arg _) = do
    typed <- transformExpression arg
    return $ S.PrefixOpExpr op typed $ getOpType op $ S.tagOfExpr typed
  where
    getOpType :: S.Operation -> S.Type -> S.Type
    getOpType op _ | op `elem` [S.ArrayLen] = S.TypeInteger 64
    getOpType op argTy | op `elem` [S.ValRef] = S.TypePointer argTy
    getOpType op (S.TypePointer ty) | op `elem` [S.PtrDeRef] = ty
    getOpType _ argType = argType

transformExpression (S.BinOpExpr S.DeRefMemberOf arg member _) = do
    -- transform into dereference + member access:
    let newArg = S.PrefixOpExpr S.PtrDeRef arg ()
        expr = S.BinOpExpr S.MemberOf newArg member ()
    transformExpression expr

transformExpression (S.BinOpExpr S.MemberOf arg (S.VarExpr name _) _) = do
    typed <- transformExpression arg
    ty <- resolveType $ S.tagOfExpr typed

    unless (isTuple ty) $ error $ "Cannot access member of: '" 
        ++ show arg ++ "', expression is not a tuple."
    
    let S.TypeTuple tupleName fields = ty
        maybeField = findField fields name

    unless (Maybe.isJust maybeField) $ error $ "Tuple '" ++ tupleName 
        ++ "' does not contain field: '" ++ name ++ "'"

    let Just (S.Field fName fType) = maybeField
    resType <- resolveType fType
    
    return $ S.BinOpExpr S.MemberOf typed (S.VarExpr name resType) resType
  where
    findField :: [S.TupleFiled] -> S.Name -> Maybe S.TupleFiled
    findField fields name = find (\(S.Field nm _) -> nm == name) fields

transformExpression e@(S.BinOpExpr S.MemberOf argA argB _) = do
    let failMsg = "Cannot use: '" ++ show argB 
            ++ "' as subscript to: '" ++ show argA ++ "'."
        failCtx = "In expression: '" ++ show e ++ "'"
    createFault F.Error failMsg failCtx
    -- check any way, perhaps there are some more issues to report:
    typedArgA <- transformExpression argA
    typedArgB <- transformExpression argB
    
    return $ S.BinOpExpr S.MemberOf typedArgA typedArgB S.TypeBottom

transformExpression e@(S.BinOpExpr op argA argB _) = do
    typedA <- transformExpression argA
    typedB <- transformExpression argB
    
    let tyA = S.tagOfExpr typedA
        tyB = S.tagOfExpr typedB
        commonTypes = findCommonImplicitTypes [tyA, tyB]

    let argType = if null commonTypes then S.TypeBottom else head commonTypes
        opType = getOpType op argType

    M.when (null commonTypes) $ do
        let msg = "Incompatilbe types: '" ++ show tyA 
                ++ "' and '" ++ show tyB ++ "' applied to operator '" 
                ++ show op ++ "'."
            ctx = "In expression: '" ++ show e ++ "'"
        createFault F.Error msg ctx

    M.when (length commonTypes > 1) $ do
        let msg = "Ambiguous implicit type cast, casting to '" 
                ++ show argType ++ "', all possible casts: " 
                ++ show commonTypes
            ctx = "In expression: '" ++ show e ++ "'"
        createFault F.Warning msg ctx
    
    castedA <- castExprImplicitly argType typedA
    castedB <- castExprImplicitly argType typedB

    return $ S.BinOpExpr op castedA castedB opType
  where
    getOpType :: S.Operation -> S.Type -> S.Type
    getOpType op _ | op `elem` [S.Eq, S.Neq, S.Lt, S.Lte, S.Gt, S.Gte] = S.TypeBoolean
    getOpType _ argType = argType

-- based on defined symbols:
transformExpression (S.VarExpr name _) = do
    let fail = error $ "Unknown variable: " ++ name
    decl <- Maybe.fromMaybe fail <$> findDecl name
    resType <- resolveType $ snd decl
    return $ S.VarExpr name resType

transformExpression e@(S.CallExpr name args _) = do
    let declFail = error $ "Unknown function: " ++ name

    decl <- Maybe.fromMaybe declFail <$> findDecl name
    expectedArgTypes <- M.mapM resolveType $ getArgTypes $ snd decl
    typedArgs <- M.forM (zip args expectedArgTypes) $ \(expr, ty) -> do 
        typed <- transformExpression expr
        castExprImplicitly ty typed
    
    actualArgTypes <- M.mapM (resolveType . S.tagOfExpr) typedArgs
    M.unless (actualArgTypes == expectedArgTypes) $
        createFault F.Error failArgTypeMsg $ failArgTypeCtx expectedArgTypes actualArgTypes

    resType <- resolveType $ getReturnType $ snd decl
    return $ S.CallExpr name typedArgs resType
  where
    getReturnType :: S.Type -> S.Type
    getReturnType (S.TypeFunction _ ret) = ret

    getArgTypes :: S.Type -> [S.Type]
    getArgTypes (S.TypeFunction args _) = args

    failArgTypeMsg = "Mismatched argument types in call of function '" ++ name ++ "'."
    failArgTypeCtx exp act = "Expected types: '" ++ show exp ++ "'\n"
                          ++ "Actual type:    '" ++ show act ++ "'\n"
                          ++ "In expression:  '" ++ show e ++ "'"

transformExpression (S.ElementOfExpr name index _) = do
    typedIndex <- transformExpression index
    let indexType = S.tagOfExpr typedIndex
    -- check if index has correct type

    --let castedIndex = if needsCast indexType (S.TypeInteger 64)
    castedIndex <- castExprImplicitly (S.TypeInteger 64) typedIndex

    let declFail = error $ "Unknown array: " ++ name
    arrayDecl <- Maybe.fromMaybe declFail <$> findDecl name
    -- check if array declaration from current scope has correct type
    M.unless (S.isArrayPointer $ snd arrayDecl) $ error $ name ++ " is not array pointer."
    
    resType <- resolveType $ innerType $ snd arrayDecl
    return $ S.ElementOfExpr name castedIndex resType
    where
        innerType :: S.Type -> S.Type
        innerType (S.TypePointer (S.TypeArray t)) = t

transformExpression (S.ValDeclExpr (S.ValBind kind name t) rawValue _) = do
    valueTy <- resolveType t
    typedValue <- transformExpression rawValue >>= castExprImplicitly valueTy
    addLocalDecl (name, valueTy)
    return $ S.ValDeclExpr (S.ValBind kind name valueTy) typedValue valueTy

transformExpression (S.ValDestructuringExpr bindings rawValue _) = do
    resolvedBindings <- M.mapM resolveBinding bindings
    let packedType = S.TypeTuple "" (map (\(S.ValBind _ n t) -> S.Field n t) resolvedBindings)

    typedValue <- transformExpression rawValue >>= castExprImplicitly packedType

    M.forM_ resolvedBindings $ \(S.ValBind _ name ty) -> addLocalDecl (name, ty)

    return $ S.ValDestructuringExpr resolvedBindings typedValue packedType

-- function declarations:
transformExpression (S.FunDeclExpr funDecl stmt _) = do
    pushScope
    
    declareArguments funDecl
    typedStmt <- transformStatement stmt
    
    popScope

    decl <- resolveFunDecl funDecl
    let ty = S.funDeclToType decl

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

brachCond :: S.Expression () -> TypeChecker (S.Expression S.Type)
brachCond cond = transformExpression cond >>= castExprImplicitly S.TypeBoolean

transformStatement :: S.Statement () -> TypeChecker (S.Statement S.Type)
transformStatement s@(S.ReturnStmt rawExpr) = do
    setCurrentStatement s
    typedExpr <- transformExpression rawExpr
    return $ S.ReturnStmt typedExpr

transformStatement s@(S.ExpressionStmt rawExpr) = do
    setCurrentStatement s
    typedExpr <- transformExpression rawExpr
    return $ S.ExpressionStmt typedExpr
    
transformStatement s@(S.BlockStmt rawStmts) = do
    setCurrentStatement s
    typedStmts <- M.forM rawStmts transformStatement
    return $ S.BlockStmt typedStmts

transformStatement s@(S.IfStmt rawCondition rawBody) = do
    setCurrentStatement s
    cond <- brachCond rawCondition
    typedBody <- transformStatement rawBody
    return $ S.IfStmt cond typedBody

transformStatement s@(S.WhileStmt rawCondition rawBody) = do
    setCurrentStatement s
    typedBody <- transformStatement rawBody
    cond <- brachCond rawCondition
    return $ S.WhileStmt cond typedBody

transformStatement s@(S.AssignmentStmt rawLhs rawRhs) = do
    setCurrentStatement s
    -- TODO: check if lhs is valid lhs expression
    lhs <- transformExpression rawLhs
    rhs <- transformExpression rawRhs >>= castExprImplicitly (S.tagOfExpr lhs)
    return $ S.AssignmentStmt lhs rhs
    
