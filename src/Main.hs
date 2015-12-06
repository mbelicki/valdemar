module Main where

import qualified Compiler as C

import qualified System.Environment as Env

main :: IO ()
main = do
    args <- Env.getArgs
    let options = C.defaultOptions
    C.compile options args

