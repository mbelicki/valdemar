module Main where

import qualified Parser as P
import qualified Emitter as E

import qualified System.Environment as Env

main :: IO ()
main
    = do
        putStrLn "hello!"
        args <- Env.getArgs
        let ast = P.parseExpr $ head args
        case ast of
            Right a -> E.generate [a] >> return ()
            Left err -> putStrLn "Parsing error."
        return ()

