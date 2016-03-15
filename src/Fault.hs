module Fault
    ( FaultLevel(Error, Warning)
    , Fault(Fault)
    , MaybeAst
    ) where

import Syntax as S

data FaultLevel = Error | Warning deriving (Eq, Show, Ord)
data Fault = Fault { faultLevel :: FaultLevel
                   , faultReason :: String
                   , faultContext :: String
                   } deriving (Eq, Show)

-- Either errors or AST and warnings
type MaybeAst a = Either [Fault] ([S.Expression a], [Fault])
