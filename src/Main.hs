module Main where

import qualified Parser as P
import qualified Emitter as E

import qualified Control.Monad as M

import qualified System.Environment as Env
import qualified System.Directory as Dir

compileFile :: String -> IO ()
compileFile path
    = do
        sourceFile <- readFile path
        let outPath = path ++ ".ll"
        let ast = P.parseModule sourceFile
        case ast of
            Right a -> M.void $ E.generate sourceFile outPath a
            Left err -> putStrLn $ "Parsing error: " ++ show err

main :: IO ()
main
    = do
        args <- Env.getArgs
        M.forM_ args $ \sourcePath -> do
            exists <- Dir.doesFileExist sourcePath
            if exists
                then compileFile sourcePath
                else putStrLn $ "File does not exist: " ++ sourcePath

