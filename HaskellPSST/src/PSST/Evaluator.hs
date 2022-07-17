module PSST.Evaluator (eval) where

import PSST.Core
import qualified Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList, union)
import Control.Monad.State
import Control.Monad.Except

evalKeyword :: [String]
evalKeyword = ["concat", "element", "extract", "replace", "replaceAll"]

eval :: Exp -> EvalState Exp
eval valExp@(ValExp val) = return valExp
eval (VarExp var) = throwError $ UnimplementedError "Evaluator"
eval (AssignmentExp var val)= throwError $ UnimplementedError "Evaluator"
eval (OperatorExp op e1 me2 me3) = throwError $ UnimplementedError "Evaluator"
eval (StateOpExp op maybeVar) = do
    
    case op of
        "clear" -> throwError $ UnimplementedError "Clearing"
        "check" -> throwError $ UnimplementedError "Checking"
        _ -> throwError $ InvalidOperation op
-- eval val@(ValExp exp) _ = Right val
-- eval aExp@(AssignmentExp var exp) _ = Right aExp
-- eval aExp _ = Right aExp
-- eval _ _ = Left $ UnimplementedError "Evaluator"
