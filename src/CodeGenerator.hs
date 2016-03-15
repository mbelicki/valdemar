{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CodeGenerator where

import qualified LLVM.General.AST as LLVM
import qualified LLVM.General.AST.Global as LLVM.Global
import qualified LLVM.General.AST.Constant as LLVM.Const
import qualified LLVM.General.AST.Attribute as LLVM.Attr
import qualified LLVM.General.AST.CallingConvention as LLVM.CallConv
import qualified LLVM.General.AST.IntegerPredicate as LLVM.IntPred
import qualified LLVM.General.AST.FloatingPointPredicate as LLVM.FloatPred

import Data.Word
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Function as Func

import Control.Monad as M
import Control.Monad.State
import Control.Applicative

type Argument = (LLVM.Type, LLVM.Name, String)

type SymbolTable = [(String, LLVM.Operand)]

type NamesMap = Map.Map String Int

data GeneratorState
    = GeneratorState
        { generatorCurrentBlock :: LLVM.Name                    -- Name of the active block to append to
        , generatorBlocks       :: Map.Map LLVM.Name BlockState -- Blocks for function
        , generatorSymbolTable  :: SymbolTable                  -- Function scope symbol table
        , generatorBlockCount   :: Int                          -- Count of basic blocks
        , generatorCount        :: Word                         -- Count of unnamed instructions
        , generatorNames        :: NamesMap                     -- Name Supply
        } deriving Show

data BlockState
    = BlockState
        { blockIndex :: Int                                 -- Block index
        , blockStack :: [LLVM.Named LLVM.Instruction]       -- Stack of instructions
        , blockTerm  :: Maybe (LLVM.Named LLVM.Terminator)  -- Block terminator
        } deriving Show

newtype CodeGenerator a = CodeGenerator { runCodeGen :: State GeneratorState a }
  deriving (Functor, Applicative, Monad, MonadState GeneratorState)

-- Module:

newtype ModuleBuilder a = ModuleBuilder { runModuleBuilder :: State LLVM.Module a }
  deriving (Functor, Applicative, Monad, MonadState LLVM.Module)

buildModule :: LLVM.Module -> ModuleBuilder a -> LLVM.Module
buildModule = flip (execState . runModuleBuilder)

emptyModule :: String -> LLVM.Module
emptyModule label = LLVM.defaultModule { LLVM.moduleName = label }

addDef :: LLVM.Definition -> ModuleBuilder ()
addDef d = do
    defs <- gets LLVM.moduleDefinitions
    modify $ \s -> s { LLVM.moduleDefinitions = defs ++ [d] }

makeParam :: Argument -> LLVM.Parameter
makeParam (t, n, _) = LLVM.Parameter t n []

declareType :: String -> LLVM.Type -> ModuleBuilder ()
declareType name ty = addDef $ LLVM.TypeDefinition (LLVM.Name name) (Just ty)

define :: LLVM.Type -> String -> [Argument] -> [LLVM.BasicBlock] -> ModuleBuilder ()
define returnType label arguments body
    = addDef $ LLVM.GlobalDefinition $ LLVM.functionDefaults
        { LLVM.Global.name        = LLVM.Name label
        , LLVM.Global.parameters  = (params, False)
        , LLVM.Global.returnType  = returnType
        , LLVM.Global.basicBlocks = body
        } where
            params = map makeParam arguments

-- hmmm isn't this basically a call to define with the last argument set to []?
external :: LLVM.Type -> String -> [Argument] -> ModuleBuilder ()
external returnType label arguments
    = addDef $ LLVM.GlobalDefinition $ LLVM.functionDefaults 
        { LLVM.Global.name        = LLVM.Name label
        , LLVM.Global.parameters  = (params, False)
        , LLVM.Global.returnType  = returnType
        , LLVM.Global.basicBlocks = []
        } where
            params = map makeParam arguments

-- Modules:

emptyGenerator :: GeneratorState
emptyGenerator = GeneratorState (LLVM.Name entryBlockName) Map.empty [] 1 0 Map.empty

executeGenerator :: CodeGenerator a -> GeneratorState
executeGenerator cg = execState (runCodeGen cg) emptyGenerator

entry :: CodeGenerator LLVM.Name
entry = gets generatorCurrentBlock

entryBlockName :: String
entryBlockName = "entry"

sortBlocks :: [(LLVM.Name, BlockState)] -> [(LLVM.Name, BlockState)]
sortBlocks = List.sortBy (compare `Func.on` (blockIndex . snd))

makeBlock :: (LLVM.Name, BlockState) -> LLVM.BasicBlock
makeBlock (name, BlockState _ stack term)
    = LLVM.BasicBlock name stack (maketerm term)
        where
            maketerm (Just x) = x
            maketerm Nothing = error $ "Block has no terminator: " ++ show name

createBlocks :: GeneratorState -> [LLVM.BasicBlock]
createBlocks genState = map makeBlock $ sortBlocks $ Map.toList (generatorBlocks genState)

addBlock :: String -> CodeGenerator LLVM.Name
addBlock blockName
    = do
        blocks     <- gets generatorBlocks
        blockCount <- gets generatorBlockCount
        names      <- gets generatorNames

        let newBlock = emptyBlock blockCount
            (qname, supply) = newUniqueName blockName names
            outName = LLVM.Name qname

        modify $ \s -> s { generatorBlocks = Map.insert outName newBlock blocks
                         , generatorBlockCount = blockCount + 1
                         , generatorNames = supply
                         }
        return outName 

setBlock :: LLVM.Name -> CodeGenerator LLVM.Name
setBlock blockName
    = do
        modify $ \s -> s { generatorCurrentBlock = blockName }
        return blockName

getBlock :: CodeGenerator LLVM.Name
getBlock = gets generatorCurrentBlock

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

modifyBlock :: BlockState -> CodeGenerator ()
modifyBlock newBlock 
    = do
        active <- gets generatorCurrentBlock
        blocks <- gets generatorBlocks
        let updatedBlocks = Map.insert active newBlock blocks
        modify $ \s -> s { generatorBlocks = updatedBlocks }

currentBlock :: CodeGenerator BlockState
currentBlock
    = do
        currentName <- gets generatorCurrentBlock
        blocks <- gets generatorBlocks
        case Map.lookup currentName blocks of
            Just x -> return x
            Nothing -> error $ "No such block: " ++ show currentName

newLocalName :: CodeGenerator Word
newLocalName
    = do
        nextIndex <- (+ 1) <$> gets generatorCount
        modify $ \s -> s { generatorCount = nextIndex }
        return nextIndex

newUniqueName :: String -> NamesMap -> (String, NamesMap)
newUniqueName name names
    = case Map.lookup name names of
        Nothing -> (name,  Map.insert name 1 names)
        Just i  -> (name ++ show i, Map.insert name (i + 1) names)

-- Instructions:

local :: LLVM.Type -> LLVM.Name -> LLVM.Operand
local = LLVM.LocalReference

global :: LLVM.Type -> LLVM.Name -> LLVM.Operand
global t = LLVM.ConstantOperand . LLVM.Const.GlobalReference t

assignLocal :: String -> LLVM.Operand -> CodeGenerator ()
assignLocal name operand = do
  locals <- gets generatorSymbolTable
  modify $ \s -> s { generatorSymbolTable = (name, operand) : locals }

getLocal :: String -> CodeGenerator LLVM.Operand
getLocal name = do
  symbols <- gets generatorSymbolTable
  case lookup name symbols of
    Just op -> return op
    Nothing -> error $ "Local variable not in scope: " ++ show name

instruction :: LLVM.Instruction -> CodeGenerator LLVM.Operand
instruction instr
    = do
        name <- newLocalName
        block <- currentBlock

        let stack = blockStack block
            ref = LLVM.UnName name

        modifyBlock $ block { blockStack = stack ++ [ref LLVM.:= instr] }
        -- TODO: the type here is ignored by Haskell bindings, but for sake of
        --       future-proofness this should probably be replaced with actual
        --       type
        return $ local LLVM.VoidType ref

terminator :: LLVM.Named LLVM.Terminator -> CodeGenerator (LLVM.Named LLVM.Terminator)
terminator term
    = do
        block <- currentBlock
        modifyBlock $ block { blockTerm = Just term }
        return term

hasTerminator :: BlockState -> Bool
hasTerminator = Maybe.isJust . blockTerm

needsTerminator :: BlockState -> Bool
needsTerminator = Maybe.isNothing . blockTerm

const :: LLVM.Const.Constant -> LLVM.Operand
const = LLVM.ConstantOperand

-- -- Floating point instructions:
fadd :: LLVM.Operand -> LLVM.Operand -> CodeGenerator LLVM.Operand
fadd a b = instruction $ LLVM.FAdd LLVM.NoFastMathFlags a b []

fsub :: LLVM.Operand -> LLVM.Operand -> CodeGenerator LLVM.Operand
fsub a b = instruction $ LLVM.FSub LLVM.NoFastMathFlags a b []

fmul :: LLVM.Operand -> LLVM.Operand -> CodeGenerator LLVM.Operand
fmul a b = instruction $ LLVM.FMul LLVM.NoFastMathFlags a b []

fdiv :: LLVM.Operand -> LLVM.Operand -> CodeGenerator LLVM.Operand
fdiv a b = instruction $ LLVM.FDiv LLVM.NoFastMathFlags a b []

floatComparison :: LLVM.FloatPred.FloatingPointPredicate 
                -> LLVM.Operand -> LLVM.Operand -> CodeGenerator LLVM.Operand
floatComparison pred a b = instruction $ LLVM.FCmp pred a b []

feq :: LLVM.Operand -> LLVM.Operand -> CodeGenerator LLVM.Operand
feq = floatComparison LLVM.FloatPred.UEQ

fneq :: LLVM.Operand -> LLVM.Operand -> CodeGenerator LLVM.Operand
fneq = floatComparison LLVM.FloatPred.UNE

flt :: LLVM.Operand -> LLVM.Operand -> CodeGenerator LLVM.Operand
flt = floatComparison LLVM.FloatPred.ULT

fgt :: LLVM.Operand -> LLVM.Operand -> CodeGenerator LLVM.Operand
fgt = floatComparison LLVM.FloatPred.UGT

fle :: LLVM.Operand -> LLVM.Operand -> CodeGenerator LLVM.Operand
fle = floatComparison LLVM.FloatPred.ULE

fge :: LLVM.Operand -> LLVM.Operand -> CodeGenerator LLVM.Operand
fge = floatComparison LLVM.FloatPred.UGE

-- -- Integer binary operations
and :: LLVM.Operand -> LLVM.Operand -> CodeGenerator LLVM.Operand
and a b = instruction $ LLVM.And a b []

or :: LLVM.Operand -> LLVM.Operand -> CodeGenerator LLVM.Operand
or a b = instruction $ LLVM.Or a b []

iadd :: LLVM.Operand -> LLVM.Operand -> CodeGenerator LLVM.Operand
iadd a b = instruction $ LLVM.Add False False a b []

isub :: LLVM.Operand -> LLVM.Operand -> CodeGenerator LLVM.Operand
isub a b = instruction $ LLVM.Sub False False a b []

imul :: LLVM.Operand -> LLVM.Operand -> CodeGenerator LLVM.Operand
imul a b = instruction $ LLVM.Mul False False a b []

idiv :: LLVM.Operand -> LLVM.Operand -> CodeGenerator LLVM.Operand
idiv a b = instruction $ LLVM.SDiv True a b []

intComparison :: LLVM.IntPred.IntegerPredicate
              -> LLVM.Operand -> LLVM.Operand -> CodeGenerator LLVM.Operand
intComparison pred a b = instruction $ LLVM.ICmp pred a b []

ieq :: LLVM.Operand -> LLVM.Operand -> CodeGenerator LLVM.Operand
ieq = intComparison LLVM.IntPred.EQ

ineq :: LLVM.Operand -> LLVM.Operand -> CodeGenerator LLVM.Operand
ineq = intComparison LLVM.IntPred.NE

ilt :: LLVM.Operand -> LLVM.Operand -> CodeGenerator LLVM.Operand
ilt = intComparison LLVM.IntPred.ULT

igt :: LLVM.Operand -> LLVM.Operand -> CodeGenerator LLVM.Operand
igt = intComparison LLVM.IntPred.UGT

ile :: LLVM.Operand -> LLVM.Operand -> CodeGenerator LLVM.Operand
ile = intComparison LLVM.IntPred.ULE

ige :: LLVM.Operand -> LLVM.Operand -> CodeGenerator LLVM.Operand
ige = intComparison LLVM.IntPred.UGE

trunc :: LLVM.Type -> LLVM.Operand -> CodeGenerator LLVM.Operand
trunc t a = instruction $ LLVM.Trunc a t []

zext :: LLVM.Type -> LLVM.Operand -> CodeGenerator LLVM.Operand
zext t a = instruction $ LLVM.ZExt a t []

-- -- Boolean logic
logNot :: LLVM.Operand -> CodeGenerator LLVM.Operand
logNot a = do 
    let false = CodeGenerator.const $ LLVM.Const.Int 1 0
    instruction $ LLVM.ICmp LLVM.IntPred.EQ a false []

-- -- Flow control:
br :: LLVM.Name -> CodeGenerator (LLVM.Named LLVM.Terminator)
br name = terminator $ LLVM.Do $ LLVM.Br name []

condBr :: LLVM.Operand -> LLVM.Name -> LLVM.Name -> CodeGenerator (LLVM.Named LLVM.Terminator)
condBr cond ifTrue ifFalse = terminator $ LLVM.Do $ LLVM.CondBr cond ifTrue ifFalse []

ret :: LLVM.Operand -> CodeGenerator (LLVM.Named LLVM.Terminator)
ret name = terminator $ LLVM.Do $ LLVM.Ret (Just name) []

retVoid :: CodeGenerator (LLVM.Named LLVM.Terminator)
retVoid = terminator $ LLVM.Do $ LLVM.Ret Nothing []

-- -- Conversions:
fptosi :: LLVM.Type -> LLVM.Operand -> CodeGenerator LLVM.Operand
fptosi t op = instruction $ LLVM.FPToSI op t []

sitofp :: LLVM.Type -> LLVM.Operand -> CodeGenerator LLVM.Operand
sitofp t op = instruction $ LLVM.SIToFP op t []

-- -- Memory and calling
call :: LLVM.Operand -> [LLVM.Operand] -> CodeGenerator LLVM.Operand
call func args
    = instruction $ LLVM.Call Nothing LLVM.CallConv.C [] (Right func) (toArgs args) [] []
      where
        toArgs :: [LLVM.Operand] -> [(LLVM.Operand, [LLVM.Attr.ParameterAttribute])]
        toArgs = map (\x -> (x, []))

alloca :: LLVM.Type -> CodeGenerator LLVM.Operand
alloca t = instruction $ LLVM.Alloca t Nothing 0 []

store :: LLVM.Operand -> LLVM.Operand -> CodeGenerator LLVM.Operand
store pointer value = instruction $ LLVM.Store False pointer value Nothing 0 []

load :: LLVM.Operand -> CodeGenerator LLVM.Operand
load pointer = instruction $ LLVM.Load False pointer Nothing 0 []

bitcast :: LLVM.Operand -> LLVM.Type -> CodeGenerator LLVM.Operand
bitcast op ty = instruction $ LLVM.BitCast op ty []

getElementPtr :: LLVM.Operand -> [LLVM.Operand] -> CodeGenerator LLVM.Operand
getElementPtr baseAddres offsets
    = instruction $ LLVM.GetElementPtr False baseAddres offsets []

