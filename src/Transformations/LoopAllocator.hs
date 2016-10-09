module Transformations.LoopAllocator
    ( transform
    ) where

import qualified Syntax as S

transform :: [S.Expression a] -> [S.Expression a]
transform = map transformExpression 

transformExpression :: S.Expression a -> S.Expression a
transformExpression (S.FunDeclExpr decl stmt t)
    = S.FunDeclExpr decl (transformStatement stmt) t
transformExpression e = e

-- return transformed expression and inner allocations
transformStatement :: S.Statement a -> S.Statement a
transformStatement (S.WhileStmt cond update body) 
    = S.BlockStmt $ allocations ++ [S.WhileStmt cond update newBody]
        where (newBody, allocations) = reallocate body

transformStatement (S.BlockStmt stmts) = S.BlockStmt $ map transformStatement stmts
transformStatement (S.IfStmt cond stmt) = S.IfStmt cond $ transformStatement stmt
transformStatement s = s


reallocate :: S.Statement a -> (S.Statement a, [S.Statement a])
reallocate (S.BlockStmt stmts)
    = (S.BlockStmt $ reverse newStmts, concat allocations)
        where 
          (newStmts, allocations)
            = foldr (\(s, a) (ss, as) -> (ss ++ [s], as ++ [a])) ([], [])
                $ map reallocate stmts

reallocate (S.IfStmt cond body)
    = (S.IfStmt cond newBody, allocs) where (newBody, allocs) = reallocate body

reallocate (S.WhileStmt cond update body)
    = (S.WhileStmt cond update newBody, allocs) where (newBody, allocs) = reallocate body

reallocate (S.ExpressionStmt (S.ValDeclExpr binding value tag))
    = (assignment, [S.ExpressionStmt $ declaration])
        where
            varName = S.bindingName binding
            assignment = S.AssignmentStmt (S.VarExpr varName tag) value
            declaration = S.ValDeclExpr binding (S.UndefinedExpr tag) tag

reallocate s = (s, [])
