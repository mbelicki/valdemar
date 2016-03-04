module Compiler
    -- high level compilation procedure
    ( compile
    -- compier optput structure
    , CompilerOptions
    , compilerOutputType
    , compilerVerbose
    , compilerOutputName
    -- output types used in options
    , OutputType
        ( OutExecutable
        , OutObjectFile
        , OutTargetAssembly
        , OutLLVMBitCode
        , OutLLVMLanguage
        )
    -- defaut options
    , defaultOptions
    ) where

import qualified Fault as F
import qualified Parser as P
import qualified Syntax as S
import qualified Emitter as E
import qualified TypeChecker as T

import qualified LLVM.General.Target as LLVM.Targ
import qualified LLVM.General.Context as LLVM.Ctx
import qualified LLVM.General.Module as LLVM.Module

import qualified Control.Monad as M
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Map as Map

import qualified System.Directory as Dir
import qualified System.Process as Proc

data OutputType = OutExecutable
                | OutObjectFile
                | OutTargetAssembly
                | OutLLVMBitCode
                | OutLLVMLanguage
                deriving (Eq, Ord, Show)

data FileType = ValdemarSource
              | ObjectFile
              -- TODO: support LLVM Language and bitcode files
              deriving (Eq, Ord, Show)

data CompilerOptions = CompilerOptions 
        { compilerOutputType :: OutputType
        , compilerOutputName :: String
        , compilerVerbose :: Bool
        } deriving (Eq, Ord, Show)

defaultOptions :: CompilerOptions
defaultOptions = CompilerOptions 
        { compilerOutputType = OutExecutable
        , compilerOutputName = "a.out"
        , compilerVerbose = False
        }

fileExtensions :: Map.Map FileType String
fileExtensions = Map.fromList 
    [ (ValdemarSource, ".val") 
    , (ObjectFile, ".o")
    ]

outFileExtensions :: Map.Map OutputType String
outFileExtensions = Map.fromList 
    [ (OutExecutable, "") 
    , (OutObjectFile, ".o")
    , (OutTargetAssembly, ".s")
    , (OutLLVMBitCode, ".bc")
    , (OutLLVMLanguage, ".ll")
    ]

outFileFormats :: Map.Map OutputType E.OutModuleFormat
outFileFormats = Map.fromList
    [ (OutExecutable,     E.FormatObjectFile) 
    , (OutObjectFile,     E.FormatObjectFile)
    , (OutTargetAssembly, E.FormatTargetAssembly)
    , (OutLLVMBitCode,    E.FormatLLVMBitCode)
    , (OutLLVMLanguage,   E.FormatLLVMLanguage)
    ]

failOnNothing :: String -> Maybe a -> a
failOnNothing message = Maybe.fromMaybe (error message)

getFileType :: String -> Maybe FileType
getFileType filename
    = Map.foldrWithKey f Nothing fileExtensions
        where
            f :: FileType -> String -> Maybe FileType -> Maybe FileType
            f filetype extension Nothing
                = if extension `List.isSuffixOf` filename
                    then Just filetype
                    else Nothing
            f _ _ something = something 

filterPathsByType :: FileType -> [String] -> [String]
filterPathsByType fileType
    = List.filter check
        where
            check :: String -> Bool
            check path = case getFileType path of
                Just foundType -> foundType == fileType
                Nothing -> False

getOutputExtension :: OutputType -> String
getOutputExtension outType
    = failOnNothing message $ Map.lookup outType outFileExtensions
        where
            typeString = show outType
            message = "Missing extension for " 
                        ++ typeString ++ " in extensions map."

getFormat :: OutputType -> E.OutModuleFormat
getFormat outType
    = failOnNothing msg $ Map.lookup outType outFileFormats
        where msg = "Fatal could not find output fromat for: " ++ show outType


getNewPath :: String -> OutputType -> String
getNewPath sourcePath outType
    = sourcePath ++ getOutputExtension outType

getModuleName :: Char -> String -> String
getModuleName pathDelimiter sourcePath
    = case List.elemIndices pathDelimiter sourcePath of
        [] -> sourcePath
        indices -> drop index sourcePath
                    where index = 1 + last indices

transformAst :: [S.Expression ()] -> F.MaybeAst S.Type
transformAst ast = do
    checked <- T.typeCheck ast
    -- TODO: other transformations here
    return checked

printFaults :: [F.Fault] -> IO ()
printFaults faults = M.forM_ faults $ \(F.Fault kind msg ctx) -> do
    putStr $ "[" ++ show kind ++ "]: "
    putStrLn msg
    putStrLn ctx
    putStrLn ""

compileAst :: ([S.Expression S.Type] -> IO a) -> F.MaybeAst S.Type -> IO ()
compileAst _ (Left faults) = printFaults faults
compileAst compile (Right (ast, faults)) = do 
    printFaults faults
    M.void $ compile ast

buildSource :: String -> CompilerOptions -> IO ()
buildSource filePath options = do
    exists <- Dir.doesFileExist filePath
    if exists
        then do
            eitherAST <- P.parseModule <$> readFile filePath
            case eitherAST of
                Right ast -> do 
                    M.when verbose $ putStrLn compileMessage
                    compileAst (E.generate format moduleName outPath) $ transformAst ast
                Left err -> putStrLn $ "Parsing error: " ++ show err
        else putStrLn $ "File: '" ++ filePath ++ "' could not be open. Skipping."
    where
        verbose = compilerVerbose options
        -- get output type from options, if building executable compile each
        -- source to object file
        outType = case compilerOutputType options of
                    OutExecutable -> OutObjectFile
                    other -> other
        format = getFormat outType
        outPath = getNewPath filePath outType
        moduleName = getModuleName '/' filePath

        compileMessage = "Compiling: '" ++ filePath ++ "' -> '" ++ outPath ++ "'"

linkAll :: String -> [String] -> IO ()
linkAll outName objectPaths
    = Proc.callProcess "ld" args
        where 
            args = objectPaths ++ osxArgs ++ output
            osxArgs = [ "/usr/lib/crt1.o", "-arch", "x86_64"
                      , "-macosx_version_min", "10.11", "-lSystem"
                      ]
            output = ["-o", outName]

compile :: CompilerOptions -> [String] -> IO ()
compile options files = do
    let outputType = compilerOutputType options
    let sources = filterPathsByType ValdemarSource files
    let objects = filterPathsByType ObjectFile files

    M.forM_ sources $ \file ->
        buildSource file options

    M.when (outputType == OutExecutable) $ do
        let allObjects = objects ++ map (++ ".o") sources
        let outName = compilerOutputName options
        linkAll outName allObjects

