module PSST.Evaluator (eval) where

import PSST.Core
import qualified Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList, union)
import Control.Monad.State
import Control.Monad.Except
import Data.List

evalKeyword :: [String]
evalKeyword = ["concat", "element", "extract", "replace", "replaceAll"]

mapListAdd var val env = case H.lookup var env of
    Just lst -> H.insert var (val:lst) env
    Nothing -> H.insert var [val] env


eval :: Exp -> EvalState Exp
eval valExp@(ValExp val) = return valExp
eval (VarExp var) = do
    env <- get
    case H.lookup var env of
      Nothing -> throwError $ VariableNotFound var
      Just exps -> return $ ValExp $ ResultVal $ intercalate "\n" (map (\e -> var ++ " = " ++ show e) exps)
eval (AssignmentExp var val) = do
    modify $ mapListAdd var val
    return $ ValExp $ ResultVal $ "Added Assignment: " ++ var ++ " to " ++ show val
eval (OperatorExp op e1 me2 me3) = throwError $ UnimplementedError "Evaluator"
eval (StateOpExp op maybeVar) = do
    env <- get
    case op of
        "clear" -> throwError $ UnimplementedError "Clearing"
        "check" -> case maybeVar of
          Nothing -> throwError $ UnimplementedError "Full Checker"
          Just var -> case H.lookup var env of
            Nothing -> throwError $ VariableNotFound var
            Just exps -> throwError $ UnimplementedError "Var Checker"
        _ -> throwError $ InvalidOperation op
-- eval val@(ValExp exp) _ = Right val
-- eval aExp@(AssignmentExp var exp) _ = Right aExp
-- eval aExp _ = Right aExp
-- eval _ _ = Left $ UnimplementedError "Evaluator"
