module PSST.Evaluator (eval) where

import PSST.Core
import qualified Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList, union)
import Control.Monad.State
import Control.Monad.Except
import Data.List
import Data.HashMap.Strict

extractOp :: Exp -> Maybe Exp -> Maybe Exp -> EvalState Exp
extractOp _ _ _ = unimplemented "Extract Operation"

checkOp:: Maybe String -> HashMap String [Exp] -> EvalState Exp
checkOp op env = unimplemented "Check Operation"

clearOp :: Maybe String -> HashMap String [Exp] -> EvalState Exp
clearOp op env = unimplemented "Clear Operation"

expOperations :: HashMap String (Exp -> Maybe Exp -> Maybe Exp -> EvalState Exp)
expOperations = fromList [ ("extract", extractOp)
                         , ("replace", extractOp)
                         , ("replaceAll", extractOp)
                         ]
stateOperations :: HashMap String (Maybe String -> HashMap String [Exp] -> EvalState Exp)
stateOperations = fromList [ ("check", checkOp)
                           , ("clear", checkOp)
                           ]

mapListAdd var val env = case H.lookup var env of
    Just lst -> H.insert var (val:lst) env
    Nothing -> H.insert var [val] env


eval :: Exp -> EvalState Exp
--- ### Value expressions --> return the expression directly
eval valExp@(ValExp val) = return valExp

--- ### Variable expressions --> return the expressions if variable is defined, or throw error
eval (VarExp var) = do
    env <- get
    case H.lookup var env of
      Nothing -> throwError $ VariableNotFound var
      Just exps -> return $ ValExp $ ResultVal $ intercalate "\n" (Prelude.map (\e -> var ++ " = " ++ show e) exps)

--- ### AssignmentExp expressions --> modify the environment with variable and value
eval (AssignmentExp var val) = do
    modify $ mapListAdd var val
    return $ ValExp $ ResultVal $ "Added Assignment: " ++ var ++ " to " ++ show val

--- ### Expression Operations --> 
eval (OperatorExp op e1 me2 me3) = case H.lookup op expOperations of
  Nothing -> throwError $ InvalidOperation op
  Just f -> f e1 me2 me3

--- ### State Operation expressions --> 
eval (StateOpExp op maybeVar) = do
    env <- get
    case H.lookup op stateOperations of
      Nothing -> throwError $ InvalidOperation op
      Just f -> f maybeVar env
