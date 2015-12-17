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

import qualified Control.Monad.Except as Except
import qualified Control.Monad as M
import qualified Data.Map as Map
import qualified Data.Word as Word
import qualified Data.Int as Int

arrayIndexType :: LLVM.Type
arrayIndexType = LLVM.IntegerType 32 

getLLVMType :: S.Type -> LLVM.Type
getLLVMType (S.TypeFloating n) = LLVM.FloatingPointType (fromIntegral n) LLVM.IEEE
getLLVMType (S.TypeInteger n) = LLVM.IntegerType (fromIntegral n)
getLLVMType  S.TypeBoolean = LLVM.IntegerType 1
getLLVMType  S.TypeUnit = LLVM.VoidType
getLLVMType (S.TypeArray ty)
    = LLVM.StructureType False 
        [arrayIndexType, LLVM.ArrayType 0 $ getLLVMType ty] 

getLLVMType (S.TypePointer ty)
    = LLVM.PointerType (getLLVMType ty) (LLVM.Addr.AddrSpace 0)
            

transformFuncArgs :: [S.FunctionArgument] -> [CG.Argument]
transformFuncArgs
    = map (\(S.FunArg name ty) -> (getLLVMType ty, LLVM.Name name, name))

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
    

generateCommonDecls :: CG.ModuleBuilder ()
generateCommonDecls =
    CG.external (getLLVMType S.TypeUnit) "bounds_check_failed" []

emitStatement :: S.Statement S.Type -> CG.CodeGenerator ()
emitStatement (S.ExpressionStmt n) = M.void $ emitExpression n
emitStatement (S.ReturnStmt n) = M.void $ emitExpression n >>= CG.ret
emitStatement (S.BlockStmt n) = M.forM_ n emitStatement
emitStatement (S.IfStmt cond body) = do
    thenBlock <- CG.addBlock "if.then"
    endBlock  <- CG.addBlock "if.end"

    condOp <- emitExpression cond
    CG.condBr condOp thenBlock endBlock

    CG.setBlock thenBlock
    emitStatement body
    block <- CG.currentBlock
    M.when (CG.needsTerminator block) $ 
        M.void $ CG.br endBlock
    
    CG.setBlock endBlock
    return ()

emitStatement (S.WhileStmt cond body) = do
    beginBlock <- CG.addBlock "while.begin"
    bodyBlock <- CG.addBlock "while.body"
    endBlock  <- CG.addBlock "while.end"

    CG.br beginBlock
    CG.setBlock beginBlock

    condOp <- emitExpression cond
    CG.condBr condOp bodyBlock endBlock

    CG.setBlock bodyBlock
    emitStatement body
    block <- CG.currentBlock
    M.when (CG.needsTerminator block) $ 
        M.void $ CG.br beginBlock
    
    CG.setBlock endBlock
    return ()

emitStatement (S.AssignmentStmt name n) = do
    op <- emitExpression n
    var <- CG.getLocal name
    CG.store var op
    return ()
    

binaryOperators = Map.fromList 
    [ (S.Add, CG.fadd)
    , (S.Sub, CG.fsub)
    , (S.Mul, CG.fmul)
    , (S.Div, CG.fdiv)
    , (S.BitAnd, CG.and)
    , (S.BitOr,  CG.or)
    , (S.Eq,  CG.feq)
    , (S.Neq, CG.fneq)
    , (S.Lt,  CG.flt)
    , (S.Gt,  CG.fgt)
    , (S.Lte, CG.fle)
    , (S.Gte, CG.fge)
    ]

unaryOperators = Map.fromList 
    [ (S.LogNot, CG.logNot)
    ]

emitExpression :: S.Expression S.Type -> CG.CodeGenerator LLVM.Operand
emitExpression (S.FloatExpr n ty) = return $ CG.const $ LLVM.Const.Float (LLVM.Float.Double n)
emitExpression (S.IntegerExpr n (S.TypeInteger bits))
    = return $ CG.const $ LLVM.Const.Int (fromIntegral bits) (toInteger n)
emitExpression (S.BooleanExpr n ty) = return $ CG.const $ LLVM.Const.Int 1 (toInteger $ fromEnum n)
emitExpression (S.VarExpr n ty) = CG.getLocal n >>= CG.load
emitExpression (S.ValDeclExpr (S.ValDecl kind name typeName n) ty) = do
    op <- emitExpression n
    ptr <- CG.alloca $ getLLVMType typeName
    CG.store ptr op
    CG.assignLocal name ptr
    return op

emitExpression (S.PrefixOpExpr op a ty)
    = case Map.lookup op unaryOperators of
        Nothing -> error "TODO: fail gracefully when operator is missing."
        Just instr -> do
            opA <- emitExpression a
            instr opA

emitExpression (S.BinOpExpr op a b ty)
    = case Map.lookup op binaryOperators of
        Nothing -> error "TODO: fail gracefully when operator is missing."
        Just instr -> do
            opA <- emitExpression a
            opB <- emitExpression b
            instr opA opB

emitExpression (S.CallExpr fun args ty) = do
    argSymbols <- M.mapM emitExpression args
    -- TODO: this seems to be ingoreed, but for sake of future-proofness
    --       VoidType below should be replaced with actual type value
    --       returned by the function
    let funSybmol = CG.global LLVM.VoidType $ LLVM.Name fun
    CG.call funSybmol argSymbols

emitExpression (S.ArrayExpr ns ty) = do
    consts <- M.forM ns $ \n -> emitConstant n
    let arrayConst = LLVM.Const.Array llvmType consts
    let countConst = LLVM.Const.Int 32 (toInteger $ length ns)
    let structConst = LLVM.Const.Struct Nothing False [countConst, arrayConst]
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

emitExpression (S.ElementOfExpr name index ty) = do
    indexOp <- emitExpression index
    arrayOp <- CG.getLocal name >>= CG.load

    let offset      = CG.const $ LLVM.Const.Int 32 0
        dataOffset  = CG.const $ LLVM.Const.Int 32 1
        countOffset = CG.const $ LLVM.Const.Int 32 0

    let emitBoudCheck = True

    M.when emitBoudCheck $ do
        failBlock <- CG.addBlock "bounds.fail"
        succBlock  <- CG.addBlock "bounds.success"
        
        countLocOp <- CG.getElementPtr arrayOp [offset, countOffset]
        countOp <- CG.load countLocOp

        truncedIndexOp <- CG.trunc indexOp $ getLLVMType $ S.TypeInteger 32
        
        compResultOp <- CG.ilt truncedIndexOp countOp 
        CG.condBr compResultOp succBlock failBlock

        CG.setBlock failBlock
        let funSybmol = CG.global LLVM.VoidType $ LLVM.Name "bounds_check_failed"
        CG.call funSybmol []
        CG.br succBlock

        M.void $ CG.setBlock succBlock

    ptrOp <- CG.getElementPtr arrayOp [offset, dataOffset, indexOp]
    CG.load ptrOp
 
    
emitConstant :: S.Expression S.Type -> CG.CodeGenerator LLVM.Const.Constant
emitConstant (S.FloatExpr n ty) = return $ LLVM.Const.Float (LLVM.Float.Double n)
emitConstant (S.IntegerExpr n ty) = return $ LLVM.Const.Int 64 (toInteger n)
emitConstant (S.BooleanExpr n ty) = return $ LLVM.Const.Int 1 (toInteger $ fromEnum n)

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

generate :: OutModuleFormat -> String -> String -> [S.Expression S.Type] -> IO LLVM.Module
generate format name outPath defs =
    LLVM.Ctx.withContext $ \context ->
        liftError $ LLVM.Targ.withHostTargetMachine $ \target ->
            liftError $ LLVM.Module.withModuleFromAST context newAst $ \m -> do
                let outFile = LLVM.Module.File outPath
                writeOutFile format m target outFile
                return newAst
            where
                mod = CG.emptyModule name
                modn = generateCommonDecls >> mapM generateSingleDef defs
                newAst = CG.buildModule mod modn

