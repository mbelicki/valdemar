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

import qualified Data.Maybe as Maybe
import qualified Data.List as List

-- udnderlying data types and state setup

type Decl = (S.Name, (S.Type, S.BindingKind))
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
        , checkerCurrentFunction :: Maybe Decl
        }

newtype TypeChecker a = TypeChecker { runTypeChecker :: State CheckerState a }
  deriving (Functor, Applicative, Monad, MonadState CheckerState)

emptyScope :: Scope
emptyScope = Scope [] [] Nothing

emptyChecker :: CheckerState
emptyChecker = CheckerState emptyScope emptyScope [] Nothing Nothing

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
            maybeDecl = List.find (\(n, _) -> n == name) decls

            maybeParentDecl = maybeParent >>= (\s -> findInScope getter s name)

-- cheker state operations:

beginFunction :: S.FunctionDeclaration -> TypeChecker ()
beginFunction rawDecl = do 
    let decl = (S.nameOfFunDecl rawDecl, (S.funDeclToType rawDecl, S.Immutable))
    modify $ \s -> s { checkerCurrentFunction = Just decl }
    pushScope

endFunction :: TypeChecker ()
endFunction = do 
    popScope
    modify $ \s -> s { checkerCurrentFunction = Nothing }

getCurrentFunction :: TypeChecker (Maybe Decl)
getCurrentFunction = gets checkerCurrentFunction

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
        context = contextPrefix ++ stmtPart
        contextPrefix = if contextStart == "" then "" else contextStart ++ "\n"
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

-- adds a fault and returns bottom if variable is not defined
fetchDeclType :: S.Name -> S.Expression a -> TypeChecker S.Type
fetchDeclType name e = do
    maybeDecl <- findDecl name
    M.unless (Maybe.isJust maybeDecl) $ do
        let msg = "Unknown variable: '" ++ name ++ "'."
            ctx = "In expression: '" ++ show e ++ "'"
        createFault F.Error msg ctx
    return $ maybe S.TypeBottom (fst . snd) maybeDecl

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
        getDecl decl@(S.FunDecl name _ _) = (name, (S.funDeclToType decl, S.Immutable))

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

        decls = Maybe.mapMaybe funcToDecl s
        aliases = Maybe.mapMaybe tupleToAlias s

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
needsCast (S.TypeTuple n1 _) (S.TypeTuple n2 _) = n1 /= n2 || n1 == ""
needsCast _ S.TypeBottom = False
needsCast actual expected = expected /= actual

hasTheSameLayout :: S.Type -> S.Type -> Bool
hasTheSameLayout  S.TypeBoolean S.TypeBoolean = True
hasTheSameLayout (S.TypeInteger n1) (S.TypeInteger n2) = n1 == n2
hasTheSameLayout (S.TypeFloating n1) (S.TypeFloating n2) = n1 == n2
hasTheSameLayout (S.TypeArray t1) (S.TypeArray t2) = hasTheSameLayout t1 t2
hasTheSameLayout (S.TypePointer t1) (S.TypePointer t2) = True
hasTheSameLayout (S.TypeTuple _ fields1) (S.TypeTuple _ fields2)
    = sameLength && mathingTypes
  where
    sameLength = length fields1 == length fields2
    mathingTypes = and $ zipWith hasTheSameLayout fs1 fs2
    stripFiledNames = map (\(S.Field _ t) -> t)
    fs1 = stripFiledNames fields1
    fs2 = stripFiledNames fields2

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
-- allow usage of single filed tuples as values of the filed's type
canCastImplicitly t1@S.TypeTuple{} t2 = canCastImplicitly (simplifyType t1) t2
canCastImplicitly (S.TypePointer t1) (S.TypePointer t2) = hasTheSameLayout t1 t2
canCastImplicitly _ _ = False

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

    castableTo to from = not (needsCast from to) || canCastImplicitly from to

    canCastTo :: [S.Type] -> S.Type -> Bool
    canCastTo ts t = all (castableTo t) ts

checkLeftHandAssignmentSide :: S.Expression a -> TypeChecker ()
checkLeftHandAssignmentSide e = do
    (possible, reason) <- canAssignTo e
    M.unless possible $ do
        let msg = "'" ++ show e ++ "' cannot be used as left hand side of the assigment."
        createFault F.Error msg reason
  where
    canAssignTo :: S.Expression a -> TypeChecker (Bool, String)
    canAssignTo (S.ElementOfExpr arr idx _) = return (True, "") -- check if array is mutable
    canAssignTo (S.VarExpr name _) = do
        maybeDecl <- findDecl name
        case maybeDecl of
            Just (_, (_, S.Immutable)) -> return (False, "Variable '" ++ name ++ "' is immutable.")
            Just (_, _) -> return (True, "")
            Nothing -> return (False, "Variable '" ++ name ++ "' is not defined.")
    canAssignTo (S.PrefixOpExpr op _ _) = return (True, "") -- check if reference or dereference
    canAssignTo _ = return (False, "")
    

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

transformExpression e@(S.BinOpExpr S.MemberOf arg (S.VarExpr name _) _) = do
    typed <- transformExpression arg
    ty <- resolveType $ S.tagOfExpr typed
    let failContext = "In expression: '" ++ show e ++ "'"

    M.unless (isTuple ty) $ do
        let msg = "Cannot access member of: '" ++ show arg 
                  ++ "', expression is not a tuple."
        createFault F.Error msg failContext
    
    let S.TypeTuple tupleName fields = if isTuple ty then ty else S.TypeTuple "" []
        maybeField = findField fields name

    M.unless (Maybe.isJust maybeField) $ do 
        let msg = "Tuple '" ++ tupleName ++ "' does not contain field: '" 
                  ++ name ++ "'."
        createFault F.Error msg failContext

    let S.Field fName fType = Maybe.fromMaybe (S.Field "" S.TypeBottom) maybeField
    resType <- resolveType fType
    
    return $ S.BinOpExpr S.MemberOf typed (S.VarExpr name resType) resType
  where
    findField :: [S.TupleFiled] -> S.Name -> Maybe S.TupleFiled
    findField fields name = List.find (\(S.Field nm _) -> nm == name) fields

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
transformExpression e@(S.VarExpr name _) = do
    resType <- fetchDeclType name e >>= resolveType
    return $ S.VarExpr name resType

transformExpression e@(S.CallExpr name args _) = do
    funType <- fetchDeclType name e

    unless (S.isFunction funType) $ do
        let msg = "Variable '" ++ name ++ "' is not a function."
        createFault F.Error msg exprCtx

    expectedArgTypes <- M.mapM resolveType $ getArgTypes funType
    let expArgsCount = length expectedArgTypes
        argsCount = length args
    M.unless (expArgsCount == argsCount) $
        createFault F.Error failArgCountMsg $ failArgCountCtx expArgsCount argsCount

    typedArgs <- M.forM (zip args expectedArgTypes) $ \(expr, ty) -> do 
        typed <- transformExpression expr
        castExprImplicitly ty typed
    
    actualArgTypes <- M.mapM (resolveType . S.tagOfExpr) typedArgs
    M.unless (actualArgTypes == expectedArgTypes) $
        createFault F.Error failArgTypeMsg $ failArgTypeCtx expectedArgTypes actualArgTypes

    resType <- resolveType $ getReturnType funType
    return $ S.CallExpr name typedArgs resType
  where
    exprCtx = "In expression: '" ++ show e ++ "'"

    getReturnType :: S.Type -> S.Type
    getReturnType (S.TypeFunction _ ret) = ret
    getReturnType _ = S.TypeBottom

    getArgTypes :: S.Type -> [S.Type]
    getArgTypes (S.TypeFunction args _) = args
    getArgTypes _ = []

    failArgCountMsg = "Wrong argument count in call of function '" ++ name ++ "'."
    failArgCountCtx n m = "Expected " ++ show n ++ " arguments, received " ++ show m ++ ".\n" ++ exprCtx
    failArgTypeMsg = "Mismatched argument types in call of function '" ++ name ++ "'."
    failArgTypeCtx exp act = "Expected types: '" ++ show exp ++ "'\n"
                          ++ "Actual type:    '" ++ show act ++ "'\n"
                          ++ exprCtx

transformExpression e@(S.ElementOfExpr arr index _) = do
    castedIndex <- castExprImplicitly (S.TypeInteger 64) =<< transformExpression index
    typedArr <- transformExpression arr
    let arrayType = S.tagOfExpr typedArr
    M.unless (S.isArrayPointer arrayType) $ do 
        let msg = "Variable '" ++ show arr ++ "' is not an array pointer."
            ctx = "In expression: '" ++ show e ++ "'"
        createFault F.Error msg ctx
    
    resType <- resolveType $ innerType arrayType
    return $ S.ElementOfExpr typedArr castedIndex resType
  where
    innerType :: S.Type -> S.Type
    innerType (S.TypePointer (S.TypeArray t)) = t
    innerType _ = S.TypeBottom

transformExpression e@(S.ValDeclExpr (S.ValBind kind name t) rawValue _) = do
    alreadyExists <- Maybe.isJust <$> findDecl name
    M.when alreadyExists $ do
        let msg = "Variable '" ++ name 
                  ++ "' has already been defined in current scope."
            ctx = "Second declaration: '" ++ show e ++ "'"
        createFault F.Error msg ctx

    rawValueType <- resolveType t
    rawTypedValue <- transformExpression rawValue 
    let valueTy = if rawValueType == S.TypeAuto 
                  then S.tagOfExpr rawTypedValue 
                  else rawValueType
    typedValue <- castExprImplicitly valueTy rawTypedValue
    
    addLocalDecl (name, (valueTy, kind))
    return $ S.ValDeclExpr (S.ValBind kind name valueTy) typedValue valueTy

transformExpression e@(S.ValDestructuringExpr bindings rawValue _) = do
    resolvedBindings <- M.mapM resolveBinding bindings
    typedRawValue <- transformExpression rawValue

    let typedBindings = infereTypes resolvedBindings $ fieldsTypes $ S.tagOfExpr typedRawValue
        packedType = S.TypeTuple "" (map (\(S.ValBind _ n t) -> S.Field n t) typedBindings)
    typedValue <- castExprImplicitly packedType typedRawValue

    M.forM_ typedBindings $ \(S.ValBind kind name ty) -> do
        alreadyExists <- Maybe.isJust <$> findDecl name
        M.when alreadyExists $ do
            let msg = "Variable '" ++ name 
                      ++ "' has already been defined in current scope."
                ctx = "Second declaration: '" ++ show e ++ "'"
            createFault F.Error msg ctx
        addLocalDecl (name, (ty, kind))

    return $ S.ValDestructuringExpr typedBindings typedValue packedType
  where
    fieldsTypes :: S.Type -> [S.Type]
    fieldsTypes (S.TypeTuple _ fields) = map (\(S.Field _ t) -> t) fields

    infereTypes :: [S.ValueBinding] -> [S.Type] -> [S.ValueBinding]
    infereTypes = zipWith infereType

    infereType :: S.ValueBinding -> S.Type -> S.ValueBinding
    infereType b@(S.ValBind k n t) ty = if t == S.TypeAuto then S.ValBind k n ty else b

-- function declarations:
transformExpression (S.FunDeclExpr funDecl stmt _) = do
    beginFunction funDecl

    declareArguments funDecl
    typedStmt <- transformStatement stmt
    
    endFunction

    decl <- resolveFunDecl funDecl
    let ty = S.funDeclToType decl

    return $ S.FunDeclExpr decl typedStmt ty
  where
    declareArguments :: S.FunctionDeclaration -> TypeChecker ()
    declareArguments (S.FunDecl _ args _)
        = M.forM_ args $ \(S.ValBind kind name ty) -> addLocalDecl (name, (ty, kind))

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
    maybeFunDecl <- getCurrentFunction
    M.unless (Maybe.isJust maybeFunDecl) $ do
        let msg = "Cannot use return statement outside function definition."
        createFault F.Error msg ""
    
    let funName = maybe "" fst maybeFunDecl
        funType = maybe S.TypeBottom (fst . snd) maybeFunDecl

    retType <- resolveType $ getRetType funType
    M.when (retType == S.TypeUnit) $ do
        let msg = "Cannot use return statement in function returning 'unit_t'."
            ctx = "Function '" ++ funName ++ "' is declared as: '" 
                  ++ show funType ++ "'"
        createFault F.Error msg ctx

    typedExpr <- transformExpression rawExpr >>= castExprImplicitly retType
    return $ S.ReturnStmt typedExpr
  where
    getRetType :: S.Type -> S.Type
    getRetType (S.TypeFunction _ t) = t
    getRetType _ = S.TypeBottom

transformStatement s@(S.ExpressionStmt rawExpr) = do
    setCurrentStatement s
    typedExpr <- transformExpression rawExpr
    return $ S.ExpressionStmt typedExpr
    
transformStatement s@(S.BlockStmt rawStmts) = do
    setCurrentStatement s
    pushScope
    typedStmts <- M.forM rawStmts transformStatement
    popScope
    return $ S.BlockStmt typedStmts

transformStatement s@(S.IfStmt rawCondition rawBody) = do
    setCurrentStatement s
    cond <- brachCond rawCondition
    typedBody <- transformStatement rawBody
    return $ S.IfStmt cond typedBody

transformStatement s@(S.WhileStmt rawCondition rawUpdate rawBody) = do
    setCurrentStatement s
    typedBody <- transformStatement rawBody
    cond <- brachCond rawCondition
    update <- maybe (return Nothing) ((M.liftM Just) <$> transformStatement) rawUpdate
    return $ S.WhileStmt cond update typedBody

transformStatement s@(S.AssignmentStmt rawLhs rawRhs) = do
    setCurrentStatement s
    checkLeftHandAssignmentSide rawLhs
        
    lhs <- transformExpression rawLhs
    rhs <- transformExpression rawRhs >>= castExprImplicitly (S.tagOfExpr lhs)
    return $ S.AssignmentStmt lhs rhs
