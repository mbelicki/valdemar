module Emitter where

import qualified Syntax as S
import qualified CodeGenerator as CG

import qualified LLVM.General.AST as LLVM
import qualified LLVM.General.AST.Constant as LLVM.Const
import qualified LLVM.General.AST.Float as LLVM.Float
import qualified LLVM.General.AST.AddrSpace as LLVM.Addr

import qualified LLVM.General.Target as LLVM.Targ
import qualified LLVM.General.Context as LLVM.Ctx
import qualified LLVM.General.Module as LLVM.Module
import qualified LLVM.General.PassManager as LLVM.Pass

import qualified Control.Monad.Except as Except
import qualified Control.Monad as M
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Word as Word
import qualified Data.Map as Map
import qualified Data.Int as Int

import Control.Applicative

arrayIndexType :: LLVM.Type
arrayIndexType = LLVM.IntegerType 32 

getLLVMType :: S.Type -> LLVM.Type
getLLVMType (S.TypeFloating n) = LLVM.FloatingPointType (fromIntegral n) LLVM.IEEE
getLLVMType (S.TypeInteger n) = LLVM.IntegerType (fromIntegral n)
getLLVMType  S.TypeBoolean = LLVM.IntegerType 1
getLLVMType  S.TypeUnit = LLVM.VoidType
getLLVMType (S.TypeUnknow name) = LLVM.NamedTypeReference $ LLVM.Name name
getLLVMType (S.TypeArray ty)
    = LLVM.StructureType False 
        [arrayIndexType, LLVM.ArrayType 0 $ getLLVMType ty] 

getLLVMType (S.TypeTuple name fields)
    = if name /= ""
      then LLVM.NamedTypeReference $ LLVM.Name name
      else LLVM.StructureType False $ map (\(S.Field _ ty) -> getLLVMType ty) fields

getLLVMType (S.TypePointer ty)
    = LLVM.PointerType (getLLVMType ty) (LLVM.Addr.AddrSpace 0)
            

transformFuncArgs :: [S.ValueBinding] -> [CG.Argument]
transformFuncArgs
    = map (\(S.ValBind _ name ty) -> (getLLVMType ty, LLVM.Name name, name))

generateSingleDef :: S.Expression S.Type -> CG.ModuleBuilder ()
generateSingleDef (S.FunDeclExpr (S.FunDecl name args retType) body ty) = do
    let funArgs =  transformFuncArgs args
    let blocks = CG.createBlocks $ CG.executeGenerator $ do
                    entry <- CG.addBlock CG.entryBlockName
                    CG.setBlock entry
                    -- allocate all variables on the stack
                    M.forM_ funArgs $ \(argType, argName, rawName) -> do
                        var <- CG.alloca argType
                        CG.store var (CG.local argType argName)
                        CG.assignLocal rawName var
                    -- emit code for body
                    emitStatement body
                    -- add ret void if returning unit_t
                    M.when (retType == S.TypeUnit) $ M.void CG.retVoid
                    --CG.ret
    let llvmType = getLLVMType retType
    CG.define llvmType name funArgs blocks

generateSingleDef (S.ExtFunDeclExpr (S.FunDecl name args retType) ty) = do 
    let funArgs = transformFuncArgs args
    let llvmType = getLLVMType retType
    CG.external llvmType name funArgs

generateSingleDef (S.NamedTupleDeclExpr name fileds _)
    = CG.declareType name $ getLLVMType $ S.TypeTuple "" $ fieldsToTypes fileds
  where
    fieldsToTypes :: [S.ValueBinding] -> [S.TupleFiled]
    fieldsToTypes = map (\(S.ValBind _ n t) -> S.Field n t)
    

generateCommonDecls :: CG.ModuleBuilder ()
generateCommonDecls = do
    bounds_defined <- CG.isGlobalDefined "bounds_check_failed"
    M.unless bounds_defined $ 
        CG.external (getLLVMType S.TypeUnit) "bounds_check_failed" []

emitStatement :: S.Statement S.Type -> CG.CodeGenerator ()
emitStatement (S.ExpressionStmt n) = M.void $ emitExprForValue n
emitStatement (S.ReturnStmt n) = M.void $ if S.tagOfExpr n == S.TypeUnit 
                                          then CG.retVoid
                                          else emitExprForValue n >>= CG.ret
emitStatement (S.BlockStmt n) = M.forM_ n emitStatement
emitStatement (S.IfStmt cond body) = do
    thenBlock <- CG.addBlock "if.then"
    endBlock  <- CG.addBlock "if.end"

    condOp <- emitExprForValue cond
    CG.condBr condOp thenBlock endBlock

    CG.setBlock thenBlock
    emitStatement body
    block <- CG.currentBlock
    M.when (CG.needsTerminator block) $ 
        M.void $ CG.br endBlock
    
    CG.setBlock endBlock
    return ()

emitStatement (S.WhileStmt cond update body) = do
    beginBlock <- CG.addBlock "while.begin"
    bodyBlock <- CG.addBlock "while.body"
    endBlock  <- CG.addBlock "while.end"

    CG.br beginBlock
    CG.setBlock beginBlock

    condOp <- emitExprForValue cond
    CG.condBr condOp bodyBlock endBlock

    CG.setBlock bodyBlock
    emitStatement body
    -- emit update statement if present:
    maybe (return ()) emitStatement update
    
    block <- CG.currentBlock
    M.when (CG.needsTerminator block) $ 
        M.void $ CG.br beginBlock
    
    CG.setBlock endBlock
    return ()

emitStatement (S.AssignmentStmt lhs rhs) = do
    val <- emitExprForValue rhs
    ptr <- emitExprForAddress lhs
    CG.store ptr val
    return ()

binaryOperators = Map.fromList 
    [ ((S.Add,    False), CG.fadd)
    , ((S.Sub,    False), CG.fsub)
    , ((S.Mul,    False), CG.fmul)
    , ((S.Div,    False), CG.fdiv)

    , ((S.Eq,     False), CG.feq)
    , ((S.Neq,    False), CG.fneq)
    , ((S.Lt,     False), CG.flt)
    , ((S.Gt,     False), CG.fgt)
    , ((S.Lte,    False), CG.fle)
    , ((S.Gte,    False), CG.fge)

    , ((S.BitAnd, True),  CG.and)
    , ((S.BitOr,  True),  CG.or)

    , ((S.Add,    True),  CG.iadd)
    , ((S.Sub,    True),  CG.isub)
    , ((S.Mul,    True),  CG.imul)
    , ((S.Div,    True),  CG.idiv)

    , ((S.Eq,     True),  CG.ieq)
    , ((S.Neq,    True),  CG.ineq)
    , ((S.Lt,     True),  CG.ilt)
    , ((S.Gt,     True),  CG.igt)
    , ((S.Lte,    True),  CG.ile)
    , ((S.Gte,    True),  CG.ige)
    ]

unaryOperators = Map.fromList 
    [ (S.LogNot, CG.logNot)
    , (S.ArrayLen, getArrayLenght)
    , (S.ValRef, getAddress)
    , (S.PtrDeRef, getValue)
    ]

getArrayLenght :: LLVM.Operand -> CG.CodeGenerator LLVM.Operand
getArrayLenght arrayOp = do
    let offset      = CG.const $ LLVM.Const.Int 32 0
        countOffset = CG.const $ LLVM.Const.Int 32 0

    countLocOp <- CG.getElementPtr arrayOp [offset, countOffset]
    countOp <- CG.load countLocOp

    CG.zext (getLLVMType $ S.TypeInteger 64) countOp

getAddress :: LLVM.Operand -> CG.CodeGenerator LLVM.Operand
getAddress valueOp = do
    let offset = CG.const $ LLVM.Const.Int 32 0
    CG.getElementPtr valueOp [offset]

getValue :: LLVM.Operand -> CG.CodeGenerator LLVM.Operand
getValue = CG.load

emitExprForAddress :: S.Expression S.Type -> CG.CodeGenerator LLVM.Operand 
emitExprForAddress (S.VarExpr n _) = CG.getLocal n
emitExprForAddress n@(S.ValDeclExpr (S.ValBind _ name _) _ _) = do
    op <- emitExprForValue n
    CG.getLocal name
emitExprForAddress (S.PrefixOpExpr S.PtrDeRef n _)
    = emitExprForValue n -- n evaluates to pointer

emitExprForAddress (S.AnonTupleExpr ns ty) = do
    ops <- M.forM ns $ \n -> emitExprForValue n
    -- allocate temporary struct
    ptr <- CG.alloca $ getLLVMType ty
    -- index operands
    let indexedOps = zip [0..] ops
    -- allocate all variables on the stack
    M.forM_ indexedOps $ \(i, op) -> do
        let offset       = CG.const $ LLVM.Const.Int 32 0
            memberOffset = CG.const $ LLVM.Const.Int 32 i
        memberPtr <- CG.getElementPtr ptr [offset, memberOffset]
        CG.store memberPtr op
    return ptr

emitExprForAddress (S.ElementOfExpr arr index ty) = do
    indexOp <- emitExprForValue index
    arrayOp <- emitExprForValue arr

    let offset      = CG.const $ LLVM.Const.Int 32 0
        dataOffset  = CG.const $ LLVM.Const.Int 32 1
        countOffset = CG.const $ LLVM.Const.Int 32 0

    let emitBoudCheck = True

    M.when emitBoudCheck $ do
        failBlock <- CG.addBlock "bounds.fail"
        succBlock  <- CG.addBlock "bounds.success"
        
        countLocOp <- CG.getElementPtr arrayOp [offset, countOffset]
        countOp <- CG.load countLocOp

        truncedIndexOp <- CG.trunc (getLLVMType $ S.TypeInteger 32) indexOp
        
        compResultOp <- CG.ilt truncedIndexOp countOp 
        CG.condBr compResultOp succBlock failBlock

        CG.setBlock failBlock
        let funSybmol = CG.global LLVM.VoidType $ LLVM.Name "bounds_check_failed"
        CG.call funSybmol []
        CG.br succBlock

        M.void $ CG.setBlock succBlock

    CG.getElementPtr arrayOp [offset, dataOffset, indexOp]

emitExprForAddress (S.BinOpExpr S.MemberOf n (S.VarExpr name _) ty) = do
    tuplePtr <- emitExprForAddress n

    let S.TypeTuple tupleName fields = S.tagOfExpr n
        maybeOffset = getFieldOffset fields name

    M.unless (Maybe.isJust maybeOffset) $ error $ "Tuple: '" ++ tupleName 
        ++ "' does not contain field: '" ++ name ++ "'"

    let Just rawOffset = maybeOffset
        offset      = CG.const $ LLVM.Const.Int 32 0
        fieldOffset = CG.const $ LLVM.Const.Int 32 $ fromIntegral rawOffset

    CG.getElementPtr tuplePtr [offset, fieldOffset]
  where
    getFieldOffset :: [S.TupleFiled] -> S.Name -> Maybe Int
    getFieldOffset fields name = List.findIndex (\(S.Field nm _) -> nm == name) fields

emitExprForAddress n@(S.CallExpr _ _ ty) = do
    op <- emitExprForValue n
    ptr <- CG.alloca $ getLLVMType ty
    CG.store ptr op
    return ptr

emitExprForAddress e = error ("Critical error: emitting for address: '" ++ show e 
                        ++ "' of type: '" ++ show (S.tagOfExpr e) ++ "'")

emitExprForValue :: S.Expression S.Type -> CG.CodeGenerator LLVM.Operand
emitExprForValue e@(S.FloatExpr   _ _) = CG.const <$> emitConstant e
emitExprForValue e@(S.IntegerExpr _ _) = CG.const <$> emitConstant e
emitExprForValue e@(S.BooleanExpr _ _) = CG.const <$> emitConstant e
emitExprForValue (S.VarExpr n ty) = CG.getLocal n >>= CG.load
emitExprForValue (S.ValDeclExpr (S.ValBind kind name typeName) n ty) = do
    ptr <- CG.alloca $ getLLVMType typeName
    CG.assignLocal name ptr
    M.unless (isUndefined n) $ do
        op <- emitExprForValue n
        M.void $ CG.store ptr op
    CG.load ptr
  where
    isUndefined S.UndefinedExpr{} = True
    isUndefined e = False

emitExprForValue (S.ValDestructuringExpr bindings n ty) = do
    op <- emitExprForValue n
    ptr <- CG.alloca $ getLLVMType ty
    CG.store ptr op
    -- index bindings
    let indexedBindings = zip [0..] bindings
    -- allocate all variables on the stack
    M.forM_ indexedBindings $ \(i, S.ValBind kind name rawType) -> do
        varPtr <- CG.alloca $ getLLVMType rawType 
        let offset       = CG.const $ LLVM.Const.Int 32 0
            memberOffset = CG.const $ LLVM.Const.Int 32 i
        memberPtr <- CG.getElementPtr ptr [offset, memberOffset]
        memberOp <- CG.load memberPtr
        CG.store varPtr memberOp
        CG.assignLocal name varPtr
    return op

emitExprForValue (S.PrefixOpExpr S.ValRef n _) = emitExprForAddress n

emitExprForValue (S.PrefixOpExpr op a ty)
    = case Map.lookup op unaryOperators of
        Nothing -> error $ "Operator is missing: " ++ show op ++ ", " ++ show ty
        Just instr -> do
            opA <- emitExprForValue a
            instr opA

emitExprForValue n@(S.BinOpExpr S.MemberOf _ _ _) = do
    ptr <- emitExprForAddress n
    CG.load ptr

emitExprForValue (S.BinOpExpr op a b ty)
    = case Map.lookup (op, isInt (S.tagOfExpr a)) binaryOperators of
        Nothing -> error $ "Operator is missing: " ++ show op ++ ", " ++ show ty
        Just instr -> do
            opA <- emitExprForValue a
            opB <- emitExprForValue b
            instr opA opB
        where
            isInt :: S.Type -> Bool
            isInt (S.TypeInteger _) = True
            isInt S.TypeBoolean = True
            isInt _ = False

emitExprForValue (S.CastExpr targetType@(S.TypeTuple _ _) n _) = do 
    let originalType = S.tagOfExpr n
    op <- emitExprForAddress n
    addr <- CG.bitcast op (getLLVMType $ S.TypePointer targetType)
    CG.load addr

emitExprForValue (S.CastExpr targetType n _) = do 
    let originalType = S.tagOfExpr n
    convert originalType targetType
  where
    conversions = Map.fromList
        [ ((S.TypeFloating 64, S.TypeInteger 64), CG.fptosi)
        , ((S.TypeFloating 32, S.TypeInteger 64), CG.fptosi)
        , ((S.TypeInteger 64, S.TypeFloating 64), CG.sitofp)
        , ((S.TypeInteger 64, S.TypeFloating 32), CG.sitofp)
        , ((S.TypeInteger 8,  S.TypeInteger 64), CG.zext)
        , ((S.TypeInteger 64, S.TypeInteger 8), CG.trunc)
        ]

    convert :: S.Type -> S.Type -> CG.CodeGenerator LLVM.Operand
    convert _ ty@(S.TypePointer _) = do 
        op <- emitExprForValue n
        CG.bitcast op $ getLLVMType ty
    convert tuple@(S.TypeTuple _ fields) scalar = do
        -- when casting from single filed tuple to scalar just take the first
        -- field
        ptr <- emitExprForAddress n
        let offset       = CG.const $ LLVM.Const.Int 32 0
            memberOffset = CG.const $ LLVM.Const.Int 32 0
        memberPtr <- CG.getElementPtr ptr [offset, memberOffset]
        CG.load memberPtr
    convert innerType outerType = do
        op <- emitExprForValue n
        case Map.lookup (innerType, outerType) conversions of
          Nothing -> error $ "Conversion is missing: " 
              ++ show innerType ++ " to " ++ show outerType
          Just instr -> instr (getLLVMType outerType) op

emitExprForValue (S.CallExpr fun args ty) = do
    argSymbols <- M.mapM emitExprForValue args
    let retType = getLLVMType ty
        funSybmol = CG.global retType $ LLVM.Name fun
    CG.call funSybmol argSymbols


emitExprForValue (S.ArrayExpr ns ty) = do
    consts <- M.forM ns $ \n -> emitConstant n
    let arrayConst = LLVM.Const.Array llvmType consts
        countConst = LLVM.Const.Int 32 (toInteger $ length ns)
        structConst = LLVM.Const.Struct Nothing False [countConst, arrayConst]
    ptr <- CG.alloca structType
    CG.store ptr $ CG.const structConst
    CG.bitcast ptr $ getLLVMType $ S.TypePointer (S.TypeArray elementType)
  where
    getBaseArrayType :: S.Type -> S.Type
    getBaseArrayType (S.TypePointer (S.TypeArray t)) =t

    elementType = getBaseArrayType ty
    llvmType = getLLVMType elementType
    arrayType = LLVM.ArrayType (fromIntegral $ length ns) llvmType
    structType = LLVM.StructureType False [arrayIndexType, arrayType]

emitExprForValue e@S.AnonTupleExpr{} = emitExprForAddress e >>= CG.load
emitExprForValue e@S.ElementOfExpr{} = emitExprForAddress e >>= CG.load
    
emitConstant :: S.Expression S.Type -> CG.CodeGenerator LLVM.Const.Constant
emitConstant (S.FloatExpr n ty) = return $ LLVM.Const.Float (LLVM.Float.Double n)
emitConstant (S.BooleanExpr n ty) = return $ LLVM.Const.Int 1 (toInteger $ fromEnum n)
emitConstant (S.IntegerExpr n (S.TypeInteger bits))
    = return $ LLVM.Const.Int (fromIntegral bits) (toInteger n)
emitConstant (S.AnonTupleExpr ns ty) = do
    consts <- M.forM ns $ \n -> emitConstant n
    return $ LLVM.Const.Struct Nothing False consts

liftError :: Except.ExceptT String IO a -> IO a
liftError = Except.runExceptT M.>=> either fail return


data OutModuleFormat
    = FormatObjectFile
    | FormatTargetAssembly
    | FormatLLVMBitCode
    | FormatLLVMLanguage

writeOutFile :: OutModuleFormat 
             -> LLVM.Module.Module 
             -> LLVM.Targ.TargetMachine
             -> LLVM.Module.File 
             -> IO ()
writeOutFile format mod target file
    = liftError $ case format of
        FormatObjectFile -> LLVM.Module.writeObjectToFile target file mod
        FormatTargetAssembly -> LLVM.Module.writeTargetAssemblyToFile target file mod
        FormatLLVMBitCode ->  LLVM.Module.writeBitcodeToFile file mod
        FormatLLVMLanguage -> LLVM.Module.writeLLVMAssemblyToFile file mod

optPasses :: LLVM.Pass.PassSetSpec
optPasses = LLVM.Pass.defaultCuratedPassSetSpec { LLVM.Pass.optLevel = Just 0 }

generate :: OutModuleFormat -> String -> String -> [S.Expression S.Type] -> IO LLVM.Module
generate format name outPath defs =
    LLVM.Ctx.withContext $ \context ->
        liftError $ LLVM.Targ.withHostTargetMachine $ \target -> --do
            --print newAst
            --putStrLn ""
            liftError $ LLVM.Module.withModuleFromAST context newAst $ \m -> do
                -- run optimizations:
                LLVM.Pass.withPassManager optPasses $ \pm -> do
                    LLVM.Pass.runPassManager pm m
                    LLVM.Module.moduleAST m

                let outFile = LLVM.Module.File outPath
                writeOutFile format m target outFile
                return newAst
            where
                mod = CG.emptyModule name
                newAst = CG.buildModule mod $ do
                    ds <- mapM generateSingleDef defs
                    generateCommonDecls
                    return ds
