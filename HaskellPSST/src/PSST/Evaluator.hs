module PSST.Evaluator where

import PSST.Core
import PSST.RTOperations
import qualified Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList, union, delete)
import Control.Monad.State
import Control.Monad.Except
import Data.List
import Data.HashMap.Strict
import Data.Maybe

orgStrVarWithExprs :: (String, [Exp]) -> String
orgStrVarWithExprs (var, exps) =  intercalate "\n" (Prelude.map (\e -> var ++ " = " ++ show e) exps)

orgStrVars :: HashMap String [Exp] -> [Char]
orgStrVars env = intercalate "\n\n" $ Prelude.map orgStrVarWithExprs (toList env)

isAllRegex :: Exp -> Bool
isAllRegex (IntExp i) = False
isAllRegex (RegexExp s) = True
isAllRegex (VarExp v) = False
isAllRegex (ResultValExp r) = False
isAllRegex (AssignmentExp var val) = False
isAllRegex (StateOpExp op mv) = False
isAllRegex (OperatorExp op e1 Nothing Nothing) = isAllRegex e1
isAllRegex (OperatorExp op e1 (Just e2) Nothing) = isAllRegex e1 && isAllRegex e2
isAllRegex (OperatorExp op e1 Nothing (Just e3)) = isAllRegex e1 && isAllRegex e3
isAllRegex (OperatorExp op e1 (Just e2) (Just e3)) = isAllRegex e1 && isAllRegex e2 && isAllRegex e3

getRegexFromExp :: Exp -> RegexNode
getRegexFromExp (RegexExp node) = node
getRegexFromExp _ = everyThingNode

concatOp :: Exp -> Maybe Exp -> Maybe Exp -> EvalState Exp
concatOp e1 (Just e2) (Just e3) = throwError $ NumOfArgumentsError "Concat" 2 3 [show e1, show e2, show e3]
concatOp e1 Nothing Nothing = throwError $ NumOfArgumentsError "Concat" 2 1 [show e1]
concatOp exp1@(VarExp var1) (Just exp2@(VarExp var2)) Nothing = do
  return $ OperatorExp "concat" exp1 (Just exp2) Nothing
concatOp (RegexExp re1) (Just (RegexExp re2)) Nothing = do
  return $ RegexExp $ wrapNodeInCaptureGroup [re1, re2]
concatOp e1 e2 e3 = throwError $ InvalidArgumentsError "Concat" [show e1, show e2, show e3]

extractOp :: Exp -> Maybe Exp -> Maybe Exp -> EvalState Exp
extractOp e1 Nothing Nothing = throwError $ NumOfArgumentsError "Extract" 3 1 ["only " ++ show e1]
extractOp e1 (Just e2) Nothing = throwError $ NumOfArgumentsError "Extract" 3 2 [show e1, show e2]
extractOp e1 Nothing (Just e2) = throwError $ NumOfArgumentsError "Extract" 3 2 [show e1, show e2]
extractOp (IntExp i) (Just (RegexExp e)) (Just (RegexExp x)) = unimplemented "Extract Operation"
extractOp e1 e2 e3 = throwError $ InvalidArgumentsError "Extract" [show e1, show e2, show e3]

replaceOp :: Exp -> Maybe Exp -> Maybe Exp -> EvalState Exp
replaceOp e1 Nothing Nothing = throwError $ NumOfArgumentsError "Replace" 3 1 ["only " ++ show e1]
replaceOp e1 (Just e2) Nothing = throwError $ NumOfArgumentsError "Replace" 3 2 [show e1, show e2]
replaceOp e1 Nothing (Just e2) = throwError $ NumOfArgumentsError "Replace" 3 2 [show e1, show e2]
replaceOp (RegexExp to_rep) (Just (RegexExp new_pat)) (Just (RegexExp x)) = unimplemented "Replace Operation"
replaceOp e1 e2 e3 = throwError $ InvalidArgumentsError "Replace" [show e1, show e2, show e3]


replaceAllOp :: Exp -> Maybe Exp -> Maybe Exp -> EvalState Exp
replaceAllOp e1 Nothing Nothing = throwError $ NumOfArgumentsError "ReplaceAll" 3 1 ["only " ++ show e1]
replaceAllOp e1 (Just e2) Nothing = throwError $ NumOfArgumentsError "ReplaceAll" 3 2 [show e1, show e2]
replaceAllOp e1 Nothing (Just e2) = throwError $ NumOfArgumentsError "ReplaceAll" 3 2 [show e1, show e2]
replaceAllOp (RegexExp to_rep) (Just (RegexExp new_pat)) (Just (RegexExp x)) = unimplemented "Replace All Operation"
replaceAllOp e1 e2 e3 = throwError $ InvalidArgumentsError "ReplaceAll" [show e1, show e2, show e3]

unionOp :: Exp -> Maybe Exp -> Maybe Exp -> EvalState Exp
-- Number of Argument Errors
unionOp e1 Nothing Nothing = throwError $ NumOfArgumentsError "Union" 2 1 ["only " ++ show e1]
unionOp e1 (Just e2) (Just e3) = throwError $ NumOfArgumentsError "Union" 2 3 [show e1, show e2, show e3]
-- Flip missing middle
unionOp e1 Nothing me3 = unionOp e1 me3 Nothing
-- Multiple Union
unionOp e1 (Just (OperatorExp "union" e2 me3 Nothing)) Nothing = do
  ue <- unionOp e2 me3 Nothing
  unionOp e1 (Just ue) Nothing
-- Single Union
unionOp (RegexExp re1) (Just (RegexExp re2)) Nothing = do
  return $ RegexExp (re1 `regexUnion` re2)
-- Invalid Expression Errors
unionOp (RegexExp re1) e2 Nothing = throwError $ InvalidArgumentsError "Union" ["Second expression is not valid regex or union: " ++ show e2]
unionOp e1 e2 Nothing = throwError $ InvalidArgumentsError "Union" ["First expression is not valid regex: " ++ show e1]

unifyOp :: Exp -> Maybe Exp -> Maybe Exp -> EvalState Exp
-- Number of Argument Errors
unifyOp e1 Nothing Nothing = throwError $ NumOfArgumentsError "Unify" 2 1 ["only " ++ show e1]
unifyOp e1 (Just e2) (Just e3) = throwError $ NumOfArgumentsError "Unify" 2 3 [show e1, show e2, show e3]
-- Flip missing middle
unifyOp e1 Nothing me3 = unifyOp e1 me3 Nothing
-- Multiple Unify
unifyOp e1 (Just (OperatorExp "unify" e2 me3 Nothing)) Nothing = do
  ue <- unifyOp e2 me3 Nothing
  unifyOp e1 (Just ue) Nothing
-- Single Unify
unifyOp (RegexExp re1) (Just (RegexExp re2)) Nothing = do
  return $ RegexExp (re1 `regexUnify` re2)
-- Invalid Expression Errors
unifyOp (RegexExp re1) e2 Nothing = throwError $ InvalidArgumentsError "Unify" ["Second expression is not valid regex or unify: " ++ show e2]
unifyOp e1 e2 Nothing = throwError $ InvalidArgumentsError "Unify" ["First expression is not valid regex: " ++ show e1]


singletonOp :: Exp -> Maybe Exp -> Maybe Exp -> EvalState Exp
singletonOp (RegexExp re1) Nothing Nothing = do
  return $ ResultValExp $ show $ isNodeSingleton re1
-- Invalid Expression Errors
singletonOp e1 Nothing Nothing = throwError $ InvalidArgumentsError "Singleton" ["Expression is not valid regex: " ++ show e1]
-- Number of Argument Errors
singletonOp _ (Just x) Nothing = throwError $ NumOfArgumentsError "Singleton" 1 2 ["2nd argument: " ++ show x]
singletonOp _ Nothing (Just x) = throwError $ NumOfArgumentsError "Singleton" 1 2 ["2nd argument: " ++show x]
singletonOp e1 (Just x) (Just y) = throwError $ NumOfArgumentsError "Singleton" 1 3 [show e1, show x, show y]

subsetOp :: Exp -> Maybe Exp -> Maybe Exp -> EvalState Exp
-- Number of Argument Errors
subsetOp e1 Nothing Nothing = throwError $ NumOfArgumentsError "Subset" 2 1 ["only " ++ show e1]
subsetOp e1 (Just e2) (Just e3) = throwError $ NumOfArgumentsError "Subset" 2 3 [show e1, show e2]
-- Flip missing middle
subsetOp e1 Nothing me3 = unifyOp e1 me3 Nothing
subsetOp (RegexExp re1) (Just (RegexExp re2)) Nothing = do
  return $ ResultValExp $ show $ re1 `isSubNode` re2
-- Invalid Expression Errors
subsetOp (RegexExp re1) e2 Nothing = throwError $ InvalidArgumentsError "Subset" ["Second expression is not valid regex: " ++ show e2]
subsetOp e1 e2 Nothing = throwError $ InvalidArgumentsError "Subset" ["First expression is not valid regex: " ++ show e1]

checkOp:: Maybe Exp -> HashMap String [Exp] -> EvalState Exp
-- Single Variable Checking
checkOp (Just (VarExp v)) env = case H.lookup v env of
  Nothing -> throwError $ VariableNotFoundError v
  Just exps -> 
    let 
      (regexExp, otherExp) = partition isAllRegex exps
      regex = Prelude.map getRegexFromExp regexExp
      finalResult = case regex of
        [] -> True --- Variable can assume that the "free" variables are always valid
        exp : exps' -> isEmpty (Data.List.foldr regexUnify exp exps')
    in 
    do
      return $ ResultValExp $ show finalResult
checkOp (Just exp) env = throwError $ InvalidArgumentsError "Check" [show exp]
-- Solve entire environment
checkOp Nothing env = unimplemented "Check/Solve Operation"

stateOp:: Maybe Exp -> HashMap String [Exp] -> EvalState Exp
stateOp (Just (VarExp v)) env = case H.lookup v env of
  Nothing -> throwError $ VariableNotFoundError v
  Just exps -> return $ ResultValExp $ orgStrVarWithExprs (v, exps)
stateOp (Just exp) env = throwError $ InvalidArgumentsError "State" [show exp]
stateOp Nothing env = return $ ResultValExp $ "\n" ++ orgStrVars env ++ "\n\n"

clearOp :: Maybe Exp -> HashMap String [Exp] -> EvalState Exp
clearOp var env = do
    case var of
      Nothing -> do
        modify $ const empty
        return $ ResultValExp "Cleared Solver State"
      Just (VarExp var) -> do
        modify $ H.delete var
        return $ ResultValExp $ "Removed " ++ show var ++ " from solver"
      Just other -> throwError $ InvalidArgumentsError "Clear" ["Expected variable, got " ++ show other]

expOperations :: HashMap String (Exp -> Maybe Exp -> Maybe Exp -> EvalState Exp)
expOperations = fromList [ ("concat", concatOp)
                         , ("extract", extractOp)
                         , ("replace", replaceOp)
                         , ("replaceAll", replaceAllOp)
                         , ("union", unionOp)
                         , ("unify", unifyOp)
                         , ("subset", subsetOp)
                         , ("singleton", singletonOp)
                         ]
stateOperations :: HashMap String (Maybe Exp -> HashMap String [Exp] -> EvalState Exp)
stateOperations = fromList [ ("check", checkOp)
                           , ("solve", checkOp)
                           , ("clear", clearOp)
                           , ("state", stateOp)
                           ]

mapListAdd var val env = case H.lookup var env of
    Just lst -> H.insert var (val:lst) env
    Nothing -> H.insert var [val] env


--- ## Eval function
eval :: Exp -> EvalState Exp
--- ### Value-like expressions --> return the expression directly
eval iExp@(IntExp i) = return iExp
eval sExp@(RegexExp s) = return sExp
eval rExp@(ResultValExp r) = return rExp

--- ### Variable expressions --> return the expressions associated with the variable if variable is defined, or throw error
eval (VarExp var) = do
    env <- get
    case H.lookup var env of
      Nothing -> throwError $ VariableNotFoundError var
      Just exps -> return $ ResultValExp $ orgStrVarWithExprs (var,exps)

--- ### AssignmentExp expressions --> modify the environment with variable and value
eval (AssignmentExp var val) = do
    modify $ mapListAdd var val
    return $ ResultValExp $ "Added Assignment: " ++ var ++ " = " ++ show val

--- ### Expression Operations --> operation done without needing state
eval (OperatorExp op e1 me2 me3) = case H.lookup op expOperations of
  Nothing -> throwError $ InvalidOperationError op
  Just f -> f e1 me2 me3

--- ### State Operation expressions --> operation done that require state
eval (StateOpExp op maybeVar) = do
    env <- get
    case H.lookup op stateOperations of
      Nothing -> throwError $ InvalidOperationError op
      Just f -> f maybeVar env
