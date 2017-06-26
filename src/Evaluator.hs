module Evaluator where

import AST

-- mb STG?

type Context = [(PHPVar, PHPVal)]

setContext :: Context -> (PHPVar, PHPVal) -> Context
setContext ((x, v):gamma) (y, w) = if x == y then (y,w):gamma else (x, v):setContext gamma (y,w)
setContext [] b = [b]

evalStmt :: Context -> PHPStmt -> Context
evalStmt gamma (SeqS stmts)    = foldl evalStmt gamma stmts
evalStmt gamma (ExpressionS e) = undefined
evalStmt gamma (IfS e s1 s2)   = undefined
evalStmt gamma (WhileS e s)    = undefined
evalStmt gamma (EchoS stmts)   = undefined

evalExpr :: Context -> PHPExpr -> (Context, PHPVal)
evalExpr gamma (LiteralE val) = undefined
evalExpr gamma (VariableE v) = undefined
evalExpr gamma (AssignE v e) =
  let (gamma', e') = (evalExpr gamma e)
  in  (setContext gamma' (v, e'), e')
evalExpr gamma (NegE e) = undefined
evalExpr gamma (NotE e) = undefined
evalExpr gamma (BinaryE op e1 e2) = undefined
