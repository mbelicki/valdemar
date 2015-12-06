module Main where

import qualified Compiler as C

import qualified Data.List as List

import qualified System.Environment as Env

options :: [(Maybe Char, String, C.CompilerOptions -> C.CompilerOptions)]
options = 
    [ ( Just 'c', "emit-object"
      , \c -> c { C.compilerOutputType = C.OutObjectFile }
      )
    , ( Just 'S', "emit-assembly"
      , \c -> c { C.compilerOutputType = C.OutTargetAssembly }
      )
    , ( Nothing, "emit-llvm-bitcode"
      , \c -> c { C.compilerOutputType = C.OutLLVMBitCode }
      )
    , ( Nothing, "emit-llvm-lang"
      , \c -> c { C.compilerOutputType = C.OutLLVMLanguage }
      )
    , ( Just 'v', "verbose"
      , \c -> c { C.compilerVerbose = True }
      )
    ]

data OptType = ShortOpt | LongOpt | NoOpt

getOptionKind :: String -> OptType
getOptionKind opt
    | "--" `List.isPrefixOf` opt = LongOpt
    | "-" `List.isPrefixOf` opt = ShortOpt
    | otherwise = NoOpt

findOptAction :: String -> C.CompilerOptions -> C.CompilerOptions
findOptAction opt
    = case getOptionKind opt of
        ShortOpt -> getAction $ List.find checkSymbol options
        LongOpt -> getAction $ List.find checkName options
        NoOpt -> id
        where
            stripOpt = dropWhile (== '-')
            optSymbol = head $ stripOpt opt
            optName = stripOpt opt

            checkSymbol (Just s, _, _ ) = s == optSymbol
            checkSymbol (Nothing, _, _ ) = False

            checkName (_, name, _ ) = name == optName
            
            getAction (Just (_, _, a)) = a
            getAction Nothing = id

parseOptions :: [String] -> (C.CompilerOptions, [String])
parseOptions = foldr checkOpt (C.defaultOptions, [])
    where
        checkOpt :: String -> (C.CompilerOptions, [String]) -> (C.CompilerOptions, [String])
        checkOpt opt (options, files)
            = case getOptionKind opt of
                NoOpt -> (options, opt : files)
                _ -> (findOptAction opt options, files)

main :: IO ()
main = do
    args <- Env.getArgs
    let (options, files) = parseOptions args
    print options
    print files
    C.compile options files

