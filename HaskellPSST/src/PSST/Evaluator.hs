module PSST.Evaluator (eval) where

import PSST.Core
import PSST.RTOperations
import qualified Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList, union, delete)
import Control.Monad.State
import Control.Monad.Except
import Data.List
import Data.HashMap.Strict

orgStrVarWithExprs :: (String, [Exp]) -> String
orgStrVarWithExprs (var, exps) =  intercalate "\n" (Prelude.map (\e -> var ++ " = " ++ show e) exps)

orgStrVars :: HashMap String [Exp] -> [Char]
orgStrVars env = intercalate "\n\n" $ Prelude.map orgStrVarWithExprs (toList env)

extractOp :: Exp -> Maybe Exp -> Maybe Exp -> EvalState Exp
extractOp _ _ _ = unimplemented "Extract Operation"

replaceOp :: Exp -> Maybe Exp -> Maybe Exp -> EvalState Exp
replaceOp _ _ _ = unimplemented "Replace Operation"

replaceAllOp :: Exp -> Maybe Exp -> Maybe Exp -> EvalState Exp
replaceAllOp _ _ _ = unimplemented "Replace All Operation"

checkOp:: Maybe String -> HashMap String [Exp] -> EvalState Exp
checkOp var env = unimplemented "Check/Solve Operation"

stateOp:: Maybe String -> HashMap String [Exp] -> EvalState Exp
stateOp var env = return $ ValExp . ResultVal $ "\n" ++ orgStrVars env ++ "\n\n"

clearOp :: Maybe String -> HashMap String [Exp] -> EvalState Exp
clearOp var env = do
    case var of
      Nothing -> do
        modify $ const empty
        return $ ValExp . ResultVal $ "Cleared Solver State"
      Just s -> do
        modify $ H.delete s
        return $ ValExp . ResultVal $ "Removed " ++ s ++ " from solver"

expOperations :: HashMap String (Exp -> Maybe Exp -> Maybe Exp -> EvalState Exp)
expOperations = fromList [ ("extract", extractOp)
                         , ("replace", replaceOp)
                         , ("replaceAll", replaceAllOp)
                         ]
stateOperations :: HashMap String (Maybe String -> HashMap String [Exp] -> EvalState Exp)
stateOperations = fromList [ ("check", checkOp)
                           , ("solve", checkOp)
                           , ("clear", clearOp)
                           , ("state", stateOp)
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
      Just exps -> return $ ValExp $ ResultVal $ orgStrVarWithExprs (var,exps)

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
