{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TypeChecker
    ( typeCheck
    ) where

import qualified Syntax as S

import Control.Monad as M
import Control.Monad.State
import Control.Applicative

-- udnderlying data types and state setup

type Decl = (S.Name, S.Type)

data Scope 
    = Scope 
        { scopeDeclarations :: [Decl] 
        , scopeParent :: Maybe Scope
        }

data CheckerState
    = CheckerState
        { checkerGlobalScope :: Scope
        , checkerLocalScope :: Scope
        }

newtype TypeChecker a = TypeChecker { runTypeChecker :: State CheckerState a }
  deriving (Functor, Applicative, Monad, MonadState CheckerState)

emptyScope :: Scope
emptyScope = Scope [] Nothing

emptyChecker :: CheckerState
emptyChecker = CheckerState emptyScope emptyScope

executeChecker :: TypeChecker a -> a
executeChecker tc = evalState (runTypeChecker tc) emptyChecker

-- scope modifications:

appendDecl :: Decl -> Scope -> Scope
appendDecl d s = s { scopeDeclarations = decls } where decls = d : scopeDeclarations s

-- cheker state mutations:

getGlobals :: TypeChecker Scope
getGlobals = gets checkerGlobalScope

getLocals :: TypeChecker Scope
getLocals = gets checkerLocalScope

addGlobalDecl :: Decl -> TypeChecker ()
addGlobalDecl d = do
    globals <- M.liftM (appendDecl d) getGlobals 
    modify $ \s -> s { checkerGlobalScope = globals }

addLocalDecl :: Decl -> TypeChecker ()
addLocalDecl d = do
    locals <- M.liftM (appendDecl d) getLocals
    modify $ \s -> s { checkerLocalScope = locals }

pushScope :: TypeChecker ()
pushScope = do
    locals <- getLocals
    let newLocals = emptyScope { scopeParent = Just locals }
    modify $ \s -> s { checkerLocalScope = newLocals }

popScope :: TypeChecker (Maybe Scope)
popScope = do
    maybeParent <- M.liftM scopeParent getLocals
    case maybeParent of
        Nothing    -> modify $ \s -> s { checkerLocalScope = emptyScope }
        Just scope -> modify $ \s -> s { checkerLocalScope = scope }
    return maybeParent

-- entry point:

typeCheck :: [S.Expression ()] -> [S.Expression S.Type]
typeCheck = executeChecker . checkTree

-- main checking method
checkTree :: [S.Expression ()] -> TypeChecker [S.Expression S.Type]
checkTree ast = do
    -- find and set all globals
    modify $ \s -> s { checkerGlobalScope = findGlobals ast }
    -- for body of each function do the typing
    undefined

findGlobals :: [S.Expression a] -> Scope
findGlobals = buildScope . map funcToDecl
    where
        typeOfDecl :: S.FunctionDeclaration -> S.Type
        typeOfDecl (S.FunDecl _ args retType)
            = S.TypeFunction (map (\(S.FunArg _ ty) -> ty) args) retType

        getDecl :: S.FunctionDeclaration -> Decl
        getDecl decl@(S.FunDecl name _ _) = (name, typeOfDecl decl)

        funcToDecl :: S.Expression a -> Decl
        funcToDecl (S.FunDeclExpr d _ _) = getDecl d
        funcToDecl (S.ExtFunDeclExpr d _ ) = getDecl d

        buildScope :: [Decl] -> Scope
        buildScope decls = emptyScope { scopeDeclarations = decls }

