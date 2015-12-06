module Compiler
    ( compile
    , OutputType
    , CompilerOptions
    , defaultOptions
    ) where

import qualified Parser as P
import qualified Emitter as E

import qualified LLVM.General.Target as LLVM.Targ
import qualified LLVM.General.Context as LLVM.Ctx
import qualified LLVM.General.Module as LLVM.Module

import qualified Control.Monad as M
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Map as Map

import qualified System.Directory as Dir

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
        { compilerOutput :: OutputType
        , compilerVerbose :: Bool
        }

defaultOptions :: CompilerOptions
defaultOptions = CompilerOptions 
        { compilerOutput = OutObjectFile
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
    = Maybe.fromMaybe failure $ Map.lookup outType outFileExtensions
        where
            typeString = show outType
            message = "Missing extension for " 
                        ++ typeString ++ " in extensions map."
            failure = error message

getNewPath :: String -> OutputType -> String
getNewPath sourcePath outType
    = sourcePath ++ getOutputExtension outType

getModuleName :: Char -> String -> String
getModuleName pathDelimiter sourcePath
    = case List.elemIndices pathDelimiter sourcePath of
        [] -> sourcePath
        indices -> drop index sourcePath
                    where index = 1 + last indices

buildSource :: String -> CompilerOptions -> IO ()
buildSource filePath options = do
    exists <- Dir.doesFileExist filePath
    if exists
        then do
            eitherAST <- M.liftM P.parseModule $ readFile filePath
            case eitherAST of
                Right ast -> M.void $ E.generate moduleName outPath ast
                Left err -> putStrLn $ "Parsing error: " ++ show err
        else putStrLn $ "File: '" ++ filePath ++ "' could not be open. Skipping."
    where
        outType = compilerOutput options
        outPath = getNewPath filePath outType
        moduleName = getModuleName '/' filePath

linkAll :: [String] -> IO ()
linkAll objectPaths
    = undefined
    -- ld test.val.o test.c.o /usr/lib/crt1.o -arch x86_64 -macosx_version_min 10.11 -o test -lSystem

compile :: CompilerOptions -> [String] -> IO ()
compile options files = do
    M.forM_ files $ \file -> do
        buildSource file options

