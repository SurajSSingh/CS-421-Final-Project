module PSST.Evaluator (eval) where

import PSST.Core

evalKeyword :: [String]
evalKeyword = ["concat", "element", "extract", "replace", "replaceAll"]

eval :: Exp -> Env -> Either Diagnostic Exp
eval val@(ValExp exp) _ = Right val
eval aExp@(AssignmentExp var exp) _ = Right aExp
eval aExp _ = Right aExp
eval _ _ = Left $ UnimplementedError "Evaluator"
