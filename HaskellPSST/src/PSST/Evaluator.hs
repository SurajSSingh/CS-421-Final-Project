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

concatOp :: Exp -> Maybe Exp -> Maybe Exp -> EvalState Exp
-- concatOp e1 (Just (OperatorExp "concat" e2 me3 Nothing)) Nothing = do
--   ce <- concatOp e2 me3 Nothing
--   concatOp e1 (Just ce) Nothing
-- concatOp e1 me2@(Just e2) Nothing = case e1 of
--   ValExp val -> case val of
--     RegexVal b rt -> case e2 of
--         ValExp val2 -> case val2 of
--           RegexVal b' rt' -> case (b, b') of
--             (True, True) ->   return $ ValExp . RegexVal True $ regexTreeSeqHelper [rt, rt']
--             (False, False) -> return $ ValExp . RegexVal False $ regexTreeSeqHelper [rt, rt']
--             _ -> return $ OperatorExp "concat" e1 me2 Nothing               --- Cannot concat regex with diff signs
--           _ -> throwError $ InvalidArgumentsError "Concat" ["Value 2 is not a valid Regex: " ++ show val2]
--         v2@(VarExp str) -> return $ OperatorExp "concat" e1 me2 Nothing     --- Cannot concat variable and regex, leave as is.
--         _ -> throwError $ InvalidArgumentsError "Concat" ["Not a valid expression to concat: " ++ show e2]
--     _ -> throwError $ InvalidArgumentsError "Concat" ["Value 1 is not a valid Regex: " ++ show val]
--   v1@(VarExp s) -> case e2 of
--       ValExp val2 -> return $ OperatorExp "concat" e1 me2 Nothing            --- Cannot concat variable and regex, leave as is.
--       v2@(VarExp str) -> return $ OperatorExp "concat" e1 me2 Nothing        --- Cannot concat two variable, leave as is.
--       _ -> throwError $ InvalidArgumentsError "Concat" ["Not a valid expression to concat: " ++ show e2]
--   _ -> throwError $ InvalidArgumentsError "Concat" ["Not a valid expression to concat: " ++ show e1]
-- concatOp e1 Nothing Nothing = throwError $ NumOfArgumentsError "Concat" 2 1 ["only " ++ show e1]
concatOp _ _ (Just x) = throwError $ NumOfArgumentsError "Concat" 2 3 [show x]
concatOp _ _ _ = unimplemented "Concat Operation"

extractOp :: Exp -> Maybe Exp -> Maybe Exp -> EvalState Exp
-- extractOp e1 (Just e2) (Just e3) = case e1 of 
--   ValExp v1 -> case v1 of 
--     IntVal n -> case e2 of
--       ValExp v2 -> case v2 of
--         rEV@(RegexVal b1 exT) -> if b1
--           then throwError $ InvalidArgumentsError "Extract" ["Extraction regex cannot be complemented: " ++ show rEV]
--           else
--           case e3 of
--             ValExp v3 -> case v3 of
--               eEV@(RegexVal b2 vrT) -> if not b2
--                 then unimplemented "Successful Extraction"
--                 else throwError $ InvalidArgumentsError "Extract" ["Regex to be extracted cannot be complemented: " ++ show eEV]
--               _ -> throwError $ InvalidArgumentsError "Extract" ["not a valid regex: " ++ show v3]
--             _ -> throwError $ InvalidArgumentsError "Extract" ["not a valid expression: " ++ show e3]
--         _ -> throwError $ InvalidArgumentsError "Extract" ["not valid integer: " ++ show v2]
--       _ -> throwError $ InvalidArgumentsError "Extract" ["not valid expression: " ++ show e2]
--     _ -> throwError $ InvalidArgumentsError "Extract" ["not valid value: " ++ show v1]
--   _ -> throwError $ InvalidArgumentsError "Extract" ["not valid expression: " ++ show e1]
-- extractOp e1 Nothing Nothing = throwError $ NumOfArgumentsError "Extract" 3 1 ["only " ++ show e1]
-- extractOp e1 (Just e2) Nothing = throwError $ NumOfArgumentsError "Extract" 3 2 [show e1, show e2]
-- extractOp e1 Nothing (Just e2) = throwError $ NumOfArgumentsError "Extract" 3 2 [show e1, show e2]
extractOp _ _ _ = unimplemented "Extract Operation"

replaceOp :: Exp -> Maybe Exp -> Maybe Exp -> EvalState Exp
replaceOp _ _ _ = unimplemented "Replace Operation"

replaceAllOp :: Exp -> Maybe Exp -> Maybe Exp -> EvalState Exp
replaceAllOp _ _ _ = unimplemented "Replace All Operation"

unionOp :: Exp -> Maybe Exp -> Maybe Exp -> EvalState Exp
-- unionOp e1 (Just (OperatorExp "union" e2 me3 Nothing)) Nothing = do
--   ue <- unionOp e2 me3 Nothing
--   unionOp e1 (Just ue) Nothing
-- unionOp e1 (Just e2) Nothing = case e1 of
--   ValExp v1 -> case v1 of
--     RegexVal b rt -> unimplemented "Union"
--     _ -> throwError $ InvalidArgumentsError "Extract" ["first value is not valid regex: " ++ show v1]
--   _ -> throwError $ InvalidArgumentsError "Extract" ["first value is not valid value: " ++ show e1]
-- unionOp e1 Nothing Nothing = throwError $ NumOfArgumentsError "Extract" 2 1 ["only " ++ show e1]
-- unionOp e1 me2 (Just e3) = throwError $ NumOfArgumentsError "Extract" 2 3 [show e1, show me2, show e3]
-- unionOp e1 Nothing Nothing = throwError $ NumOfArgumentsError "Extract" 2 1 ["only " ++ show e1]
unionOp _ _ _ = unimplemented "Union Operation"

unifyOp :: Exp -> Maybe Exp -> Maybe Exp -> EvalState Exp
unifyOp e1 (Just (OperatorExp "unify" e2 me3 Nothing)) Nothing = do
  ue <- unifyOp e2 me3 Nothing
  unifyOp e1 (Just ue) Nothing
unifyOp _ _ _ = unimplemented "Unify Operation"

singletonOp :: Exp -> Maybe Exp -> Maybe Exp -> EvalState Exp
-- singletonOp e1 Nothing Nothing = case e1 of
--   ValExp val -> case val of
--     RegexVal b rt -> return $ ValExp . ResultVal . show $ if b then not (isRegexSingleton rt) else isRegexSingleton rt
--     _ -> throwError $ InvalidArgumentsError "Singleton" ["Not a valid regex for singleton: " ++ show val]
--   _ -> throwError $ InvalidArgumentsError "Singleton" ["Not a valid expression for singleton: " ++ show e1]
-- singletonOp _ (Just x) Nothing = throwError $ NumOfArgumentsError "Singleton" 1 2 ["2nd argument: " ++ show x]
-- singletonOp _ Nothing (Just x) = throwError $ NumOfArgumentsError "Singleton" 1 2 ["2nd argument: " ++show x]
-- singletonOp _ (Just x) (Just y) = throwError $ NumOfArgumentsError "Singleton" 1 3 [show x, show y]
singletonOp _ _ _ = unimplemented "Singleton Operation"

--- TODO: Check that subset works correctly for complements
subsetOp :: Exp -> Maybe Exp -> Maybe Exp -> EvalState Exp
-- subsetOp e1 (Just e2) Nothing = case e1 of
--   ValExp v1 -> case v1 of
--     RegexVal b1 rt1 -> case e2 of
--       ValExp v2 -> case v2 of 
--         RegexVal b2 rt2 -> case (b1, b2) of 
--           -- ex1: "a" is subset "a+"
--           -- ex2: "b" not subset "a+"
--           (False, False) -> return $ ValExp . ResultVal . show $ isRegexSubLang rt1 rt2
--           -- ex1: "a" not subset ~"a+"
--           -- ex2: "b" is subset ~"a+"
--           (False, True) -> return $ ValExp . ResultVal . show . not $ isRegexSubLang rt1 rt2
--           -- ex1: ~"a" not subset "a+" and "a+" not subset ~"a"
--           -- ex2: ~"b" not subset "a+" but "a+" is subset "~b" 
--           (True, False) -> return $ ValExp . ResultVal . show $ not (isRegexSubLang rt1 rt2) && not (isRegexSubLang rt2 rt1)
--           -- ex1: ~"a" not subset ~"a+" and ~"a+" not subset ~"a"
--           -- ex2: ~"b" not subset ~"a+" but ~"a+" is subset ~"b"
--           (True, True) -> return $ ValExp . ResultVal . show $ not` (isRegexSubLang rt1 rt2) && not (isRegexSubLang rt2 rt1)
--         _ -> throwError $ InvalidArgumentsError "Subset" ["Not a valid regex for superset: " ++ show e2]
--       _ -> throwError $ InvalidArgumentsError "Subset" ["Not a valid expression for superset: " ++ show e2]
--     _ -> throwError $ InvalidArgumentsError "Subset" ["Not a valid regex for subset: " ++ show e1]
--   _ -> throwError $ InvalidArgumentsError "Subset" ["Not a valid expression for subset: " ++ show e1]
-- subsetOp e1 Nothing Nothing = throwError $ NumOfArgumentsError "Subset" 2 1 ["only " ++ show e1]
-- subsetOp _ _ (Just x) = throwError $ NumOfArgumentsError "Concat" 2 3 [show x]
subsetOp _ _ _ = unimplemented "subsetOp Operation"

checkOp:: Maybe Exp -> HashMap String [Exp] -> EvalState Exp
checkOp var env = unimplemented "Check/Solve Operation"

stateOp:: Maybe Exp -> HashMap String [Exp] -> EvalState Exp
stateOp var env = return $ ResultValExp $ "\n" ++ orgStrVars env ++ "\n\n"

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
                         , ("singleton", singletonOp)
                         , ("subset", subsetOp)
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


eval :: Exp -> EvalState Exp
--- ### Value-like expressions --> return the expression directly
eval iExp@(IntExp i) = return iExp
eval sExp@(RegexExp s) = return sExp
eval rExp@(ResultValExp r) = return rExp

--- ### Variable expressions --> return the expressions if variable is defined, or throw error
eval (VarExp var) = do
    env <- get
    case H.lookup var env of
      Nothing -> throwError $ VariableNotFoundError var
      Just exps -> return $ ResultValExp $ orgStrVarWithExprs (var,exps)

--- ### AssignmentExp expressions --> modify the environment with variable and value
eval (AssignmentExp var val) = do
    modify $ mapListAdd var val
    return $ ResultValExp $ "Added Assignment: " ++ var ++ " to " ++ show val

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
