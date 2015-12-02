module Emitter where

import qualified Syntax as S
import qualified CodeGenerator as CG

import qualified LLVM.General.AST as LLVM
import qualified LLVM.General.AST.Constant as LLVM.Const
import qualified LLVM.General.AST.Float as LLVM.Float

import qualified LLVM.General.Context as LLVM.Ctx
import qualified LLVM.General.Module as LLVM.Module

import qualified Control.Monad.Except as Except
import qualified Control.Monad as M
import qualified Data.Map as Map
import qualified Data.Word as Word
import qualified Data.Int as Int

type Fallible a = Either String a

isSuccesful :: Fallible a -> Bool
isSuccesful = not . isFailure

isFailure :: Fallible a -> Bool
isFailure (Left _)  = True
isFailure (Right _) = False

failFromMaybe :: String -> Maybe a -> Fallible a
failFromMaybe message maybeValue
    = case maybeValue of
        Just a -> Right a
        Nothing -> Left message

builtInTypes :: Map.Map String LLVM.Type
builtInTypes
    = Map.fromList 
        [ ("int_t",    CG.int)
        , ("double_t", CG.double)
        ]

transformArgument :: S.FunArg -> Fallible CG.Argument
transformArgument (S.FunArg name rawType)
    = failFromMaybe message maybeArgument
        where 
            message = "Unknown type: " ++ rawType
            maybeType = Map.lookup rawType builtInTypes
            maybeArgument = M.liftM (\a -> (a, LLVM.Name name, name)) maybeType

transformFuncArgs :: [S.FunArg] -> Fallible [CG.Argument]
transformFuncArgs
    = foldr fold (Right [])
        where
            fold :: S.FunArg -> Fallible [CG.Argument] -> Fallible [CG.Argument] 
            fold rawArg otherArgs
                = let pair = (transformArgument rawArg, otherArgs)
                  in case pair of
                    (Left m,    Right _) -> Left m
                    (Left m1,   Left m2) -> Left (m1 ++ "\n" ++ m2)
                    (Right _,   Left m) -> Left m
                    (Right arg, Right args) -> Right (args ++ [arg])

generateSingleDef :: S.Expr -> CG.ModuleBuilder ()
generateSingleDef (S.FunDecl name args retType body)
    = case transformFuncArgs args of
        Left _ -> return () -- return this error higher
        Right funArgs -> do
            let blocks = CG.createBlocks $ CG.executeGenerator $ do
                    entry <- CG.addBlock CG.entryBlockName
                    CG.setBlock entry
                    -- allocate all variables on the stack
                    M.forM_ funArgs $ \(argType, argName, rawName) -> do
                        var <- CG.alloca argType
                        CG.store var (CG.local argType argName)
                        CG.assignLocal rawName var
                    -- emmit code for each statement
                    M.forM_ body $ \s -> emmitStatement s
                    --CG.ret
            CG.define CG.double name funArgs blocks

generateSingleDef (S.ExtFunDecl name args retType)
    = case transformFuncArgs args of
        Left _ -> return () -- return this error higher
        Right funArgs -> CG.external CG.double name funArgs
    

emmitStatement :: S.Statement -> CG.CodeGenerator ()
emmitStatement (S.Return n)
    = do
        emmitExpression n >>= CG.ret
        return ()

operators = Map.fromList 
    [ (S.Add, CG.fadd)
    , (S.Sub, CG.fsub)
    , (S.Mul, CG.fmul)
    , (S.Div, CG.fdiv)
    ]

emmitExpression :: S.Expr -> CG.CodeGenerator LLVM.Operand
emmitExpression (S.Float n) = return $ CG.const $ LLVM.Const.Float (LLVM.Float.Double n)
emmitExpression (S.Integer n) = return $ CG.const $ LLVM.Const.Int 64 (toInteger n)
emmitExpression (S.Var n) = CG.getLocal n >>= CG.load
emmitExpression (S.BinOp op a b)
    = case Map.lookup op operators of
        Nothing -> error "TODO: fail gracefully when operator is missing."
        Just instr ->
            do
                opA <- emmitExpression a
                opB <- emmitExpression b
                instr opA opB
emmitExpression (S.Call fun args)
    = do
        argSymbols <- M.mapM emmitExpression args
        let funSybmol = CG.extern CG.double $ LLVM.Name fun
        CG.call funSybmol argSymbols

generate :: String -> String -> [S.Expr] -> IO LLVM.Module
generate name outPath defs = LLVM.Ctx.withContext $ \context ->
  liftError $ LLVM.Module.withModuleFromAST context newAst $ \m -> do
    llstr <- LLVM.Module.moduleLLVMAssembly m
    putStrLn llstr
    let outFile = LLVM.Module.File outPath
    liftError $ LLVM.Module.writeLLVMAssemblyToFile outFile m
    return newAst
  where
    liftError :: Except.ExceptT String IO a -> IO a
    liftError = Except.runExceptT M.>=> either fail return

    mod = CG.emptyModule name
    modn = mapM generateSingleDef defs
    newAst = CG.buildModule mod modn

