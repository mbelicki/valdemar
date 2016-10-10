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

import qualified Transformations.LoopAllocator as LoopAllocator

import qualified LLVM.General.Target as LLVM.Targ
import qualified LLVM.General.Context as LLVM.Ctx
import qualified LLVM.General.Module as LLVM.Module

import qualified Control.Monad as M
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Map as Map

import qualified System.Directory as Dir
import qualified System.Process as Proc
import qualified System.Exit as Exit
import qualified System.Info as Info

import Control.Applicative

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
    (tree, faults) <- T.typeCheck ast
    let transformed = LoopAllocator.transform tree
    return $ (transformed, faults)

applyControlCodes :: String -> String -> String
applyControlCodes code string = "\x1b" ++ code ++ string ++ "\x1b[0m"

showFaultLevel :: F.FaultLevel -> Bool -> String
showFaultLevel level useColor
    = if useColor then applyControlCodes colorCode name else name
  where
    name = map Char.toLower $ show level
    colorCode = case level of
        F.Error -> "[31;1m"
        F.Warning -> "[33;1m"

printFaults :: [F.Fault] -> IO ()
printFaults faults = M.forM_ faults $ \(F.Fault kind msg ctx) -> do
    putStr $ "[" ++ showFaultLevel kind True ++ "]: "
    putStrLn $ applyControlCodes "[1m" msg
    putStrLn ctx
    putStrLn ""

printParseError :: Show a => a -> IO ()
printParseError err = printFaults [fault]
    where fault = F.Fault F.Error "Parsing failed." $ show err

compileAst :: ([S.Expression S.Type] -> IO a) -> F.MaybeAst S.Type -> IO Bool
compileAst _ (Left faults) = printFaults faults >> return False
compileAst compile (Right (ast, faults)) = do 
    printFaults faults
    M.void $ compile ast
    return True

buildSource :: String -> CompilerOptions -> IO Bool
buildSource filePath options = do
    exists <- Dir.doesFileExist filePath
    if exists
        then do
            eitherAST <- P.parseModule <$> return filePath <*> readFile filePath 
            case eitherAST of
                Right ast -> do 
                    M.when verbose $ putStrLn compileMessage
                    compileAst (E.generate format moduleName outPath) $ transformAst ast
                Left err -> do 
                    printParseError err
                    return False
        else do 
            putStrLn $ "File: '" ++ filePath ++ "' could not be open." 
            return False
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

checkFiles :: [String] -> IO (Bool, [String])
checkFiles paths = do
    results <- M.forM paths $ \path -> do
        exists <- Dir.doesFileExist path 
        return $ if exists then Nothing else Just path
    
    let notExisting = Maybe.catMaybes results
    return (null notExisting, notExisting)

linkAll :: String -> [String] -> CompilerOptions -> IO (Bool, [F.Fault])
linkAll outName objectPaths options = do
    M.when (compilerVerbose options) $ putStrLn ("Linking: " ++ outName)
    let requiredFiles = case Info.os of
                    "linux" -> [ "/lib64/ld-linux-x86-64.so.2", "/usr/lib/x86_64-linux-gnu/crti.o"
                               , "/usr/lib/x86_64-linux-gnu/crt1.o", "/usr/lib/x86_64-linux-gnu/crtn.o"
                               ]
                    "darwin" -> ["/usr/lib/crt1.o"]
                    _ -> []

    (ok, notExisting) <- checkFiles requiredFiles
    if ok then do
        let args = objectPaths ++ ldArgs ++ output
            ldArgs = if Info.os == "linux" then gnuArgs else osxArgs
            osxArgs = [ "/usr/lib/crt1.o", "-arch", "x86_64"
                      , "-macosx_version_min", "10.11", "-lSystem"
                      ]
            gnuArgs = [ "-dynamic-linker", "/lib64/ld-linux-x86-64.so.2"
                      , "/usr/lib/x86_64-linux-gnu/crti.o", "/usr/lib/x86_64-linux-gnu/crt1.o"
                      , "-lc", "-lm", "/usr/lib/x86_64-linux-gnu/crtn.o"
                      ]
            output = ["-o", outName]
        --Proc.callProcess "ld" args
        (code, stdout, stderr) <- Proc.readProcessWithExitCode "ld" args ""
        let success = code == Exit.ExitSuccess
            reason = "Command: ld " ++ (concat $ List.intersperse " " args)
                  ++ "\nstdout:\n" ++ stdout
                  ++ "\nstderr:\n" ++ stderr
        return (success, if success then [] else [F.Fault F.Error "Linking failed." reason])
    else do
        let reason = "Cannot access following files:\n" ++ (concat $ List.intersperse "\n" notExisting)
        return (False, [F.Fault F.Error "Linking failed." reason])


compile :: CompilerOptions -> [String] -> IO ()
compile options files = do
    let outputType = compilerOutputType options
    let sources = filterPathsByType ValdemarSource files
    let objects = filterPathsByType ObjectFile files

    results <- M.forM sources $ \file ->
        buildSource file options

    let somethingFailed = False `elem` results

    M.when (outputType == OutExecutable && not somethingFailed) $ do
        let allObjects = objects ++ map (++ ".o") sources
        let outName = compilerOutputName options
        (_, faults) <- linkAll outName allObjects options
        printFaults faults

    M.when (outputType == OutExecutable && somethingFailed) $ do
        putStrLn "Compilation failed."
        Exit.exitFailure

