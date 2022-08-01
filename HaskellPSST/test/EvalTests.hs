module EvalTests (evaluatorTests) where

import PSST.Core
import PSST.Evaluator
import Data.HashMap.Strict as H
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Control.Monad.Except
import Control.Monad.State
import PSST.Evaluator

regexAorB :: Exp
regexAorB = RegexExp $ wrapNodeInCaptureGroup [ChoiceNode (LiteralNode $ Right "a") (LiteralNode $ Right "b")]
regexAStar :: Exp
regexAStar = RegexExp $ wrapNodeInCaptureGroup [RepetitionNode False 0 Nothing $ LiteralNode $ Right "a"]
regexAnyChar :: Exp
regexAnyChar = RegexExp $ wrapNodeInCaptureGroup [LiteralNode $ Left True]
regexNotB :: Exp
regexNotB = RegexExp $ wrapNodeInCaptureGroup [ComplementNode $ LiteralNode $ Right "b"]

envEmpty :: Env
envEmpty = H.fromList []

envSingleVar :: Env
envSingleVar = H.fromList [ ("x", [regexAorB]) ]

envTwoVar :: Env
envTwoVar = H.fromList [ ("x", [regexAorB])
                       , ("y", [regexNotB])
                       ]

envSingleVarWithMultiple :: Env
envSingleVarWithMultiple = H.fromList [ ("x", [regexAStar, regexAorB]) ]

envMultipleVarWithMultiple :: Env
envMultipleVarWithMultiple = H.fromList [ ("x", [regexAStar, regexAorB])
                                        , ("y", [regexAnyChar])
                                        ]

evaluatorTests :: TestTree
evaluatorTests = testGroup "Evaluator Tests" [evalCorrectTest, evalDiagnosticTest]

evalCorrectTest :: TestTree
evalCorrectTest = testGroup "Evaluator Correct Tests" [evalSimpleExpCorrectTests, evalAssignmentExpTests, evalClearOpExpTest, evalStateOpExpTest]

evalSimpleExpCorrectTests :: TestTree
evalSimpleExpCorrectTests = testGroup "Simple Exp Eval Tests"
    [ testCase "Integer Expression" $ runExcept (runStateT (eval (IntExp 10)) envEmpty) @?= Right (IntExp 10, envEmpty)
    , testCase "Regex Expression" $ runExcept (runStateT (eval regexAnyChar) envEmpty) @?= Right (regexAnyChar, envEmpty)
    , testCase "Var Expression" $ runExcept (runStateT (eval $ VarExp "x") envSingleVar) @?= Right (ResultValExp $ orgStrVarWithExprs ("x", [regexAorB]), envSingleVar)
    ]

evalAssignmentExpTests :: TestTree
evalAssignmentExpTests = testGroup "Assignment Exp Eval Tests"
    [ testCase "Assign to Empty" $ runExcept (runStateT (eval (AssignmentExp "x" regexAorB)) envEmpty) @?= Right (ResultValExp $ "Added Assignment: x = " ++ show regexAorB, envSingleVar)
    , testCase "Assign Another Exp to same Var" $ runExcept (runStateT (eval (AssignmentExp "x" regexAorB)) envEmpty) @?= Right (ResultValExp $ "Added Assignment: x = " ++ show regexAorB, envSingleVar)
    , testCase "Assign Another Exp to same Var" $ runExcept (runStateT (eval (AssignmentExp "x" regexAStar)) envSingleVar) @?= Right (ResultValExp $ "Added Assignment: x = " ++ show regexAStar, envSingleVarWithMultiple)
    , testCase "Assign Another Var" $ runExcept (runStateT (eval (AssignmentExp "y" regexNotB)) envSingleVar) @?= Right (ResultValExp $ "Added Assignment: y = " ++ show regexNotB, envTwoVar)
    , testCase "Assign Another Var with Multiple" $ runExcept (runStateT (eval (AssignmentExp "y" regexAnyChar)) envSingleVarWithMultiple) @?= Right (ResultValExp $ "Added Assignment: y = " ++ show regexAnyChar, envMultipleVarWithMultiple)
    ]

evalClearOpExpTest :: TestTree
evalClearOpExpTest = testGroup "Clear Operators Eval Tests"
    [ testCase "Var Clear Operation - single-exp var" $ runExcept (runStateT (eval $ StateOpExp "clear" (Just $ VarExp "y")) envMultipleVarWithMultiple) @?= Right (ResultValExp "Removed \"y\" from solver", envSingleVarWithMultiple)
    , testCase "Var Clear Operation - multi-exp var" $ runExcept (runStateT (eval $ StateOpExp "clear" (Just $ VarExp "x")) envSingleVarWithMultiple) @?= Right (ResultValExp "Removed \"x\" from solver", envEmpty)
    , testCase "Full Clear Operation" $ runExcept (runStateT (eval $ StateOpExp "clear" Nothing) envMultipleVarWithMultiple) @?= Right (ResultValExp "Cleared Solver State", envEmpty)
    ]

evalStateOpExpTest :: TestTree
evalStateOpExpTest = testGroup "State Operators Eval Tests"
    [ testCase "Var State Operation" $ runExcept (runStateT (eval $ StateOpExp "state" (Just $ VarExp "x")) envMultipleVarWithMultiple) @?= Right (ResultValExp $ orgStrVarWithExprs ("x", [regexAStar, regexAorB]), envMultipleVarWithMultiple)
    , testCase "Full State Operation" $ runExcept (runStateT (eval $ StateOpExp "state" Nothing) envMultipleVarWithMultiple) @?= Right (ResultValExp $ "\n" ++ orgStrVars envMultipleVarWithMultiple ++ "\n\n", envMultipleVarWithMultiple)
    ]

evalDiagnosticTest :: TestTree
evalDiagnosticTest = testGroup "Evaluator Diagnostic Tests" [evalDiagnosticNoVarTest, evalDiagnosticInvalidOpTest, evalDiagnosticInvalidArgTest, evalDiagnosticNumberMismatchTest]

evalDiagnosticNoVarTest :: TestTree
evalDiagnosticNoVarTest = testGroup "No Var Found"
    [ testCase "Direct Call" $ runExcept (runStateT (eval $ VarExp "y") envSingleVar) @?= Left (VariableNotFoundError "y")
    , testCase "State Command" $ runExcept (runStateT (eval $ StateOpExp "state" $ Just$  VarExp "y") envSingleVar) @?= Left (VariableNotFoundError "y")
    , testCase "Check/Solve Command" $ runExcept (runStateT (eval $ StateOpExp "check" $ Just$  VarExp "y") envSingleVar) @?= Left (VariableNotFoundError "y")
    ]

evalDiagnosticInvalidOpTest :: TestTree
evalDiagnosticInvalidOpTest = testGroup "Invalid Operation"
    [ testCase "Expression Operation" $ runExcept (runStateT (eval $ OperatorExp "hello" (VarExp "y") Nothing Nothing) envSingleVar) @?= Left (InvalidOperationError "hello")
    , testCase "State Operation" $ runExcept (runStateT (eval $ StateOpExp "world" Nothing) envSingleVar) @?= Left (InvalidOperationError "world")
    ]

evalDiagnosticInvalidArgTest :: TestTree
evalDiagnosticInvalidArgTest = testGroup "Invalid Argument"
    [ testCase "Concat Operation" $ runExcept (runStateT (eval $ OperatorExp "concat" (IntExp 10) (Just $ VarExp "y") Nothing) envSingleVar) @?= Left (InvalidArgumentsError "Concat" [])
    , testCase "Extract Operation" $ runExcept (runStateT (eval $ OperatorExp "extract" (IntExp 10) (Just $ VarExp "x") (Just $ VarExp "y")) envSingleVar) @?= Left (InvalidArgumentsError "Extract" [])
    , testCase "Replace Operation" $ runExcept (runStateT (eval $ OperatorExp "replace" (IntExp 10) (Just $ VarExp "x") (Just $ VarExp "y")) envSingleVar) @?= Left (InvalidArgumentsError "Replace" [])
    , testCase "ReplaceAll Operation" $ runExcept (runStateT (eval $ OperatorExp "replaceAll" (IntExp 10) (Just $ VarExp "x") (Just $ VarExp "y")) envSingleVar) @?= Left (InvalidArgumentsError "ReplaceAll" [])
    , testCase "Union Operation" $ runExcept (runStateT (eval $ OperatorExp "union" (VarExp "x") (Just $ VarExp "y") Nothing) envSingleVar) @?= Left (InvalidArgumentsError "Union" [])
    , testCase "Unify Operation" $ runExcept (runStateT (eval $ OperatorExp "unify" (VarExp "x") (Just $ VarExp "y") Nothing) envSingleVar) @?= Left (InvalidArgumentsError "Unify" [])
    , testCase "Subset Operation" $ runExcept (runStateT (eval $ OperatorExp "subset" (IntExp 10) (Just $ IntExp 20) Nothing) envSingleVar) @?= Left (InvalidArgumentsError "Subset" [])
    , testCase "Singleton Operation" $ runExcept (runStateT (eval $ OperatorExp "singleton" (IntExp 10) Nothing Nothing) envSingleVar) @?= Left (InvalidArgumentsError "Singleton" [])
    ]

evalDiagnosticNumberMismatchTest :: TestTree
evalDiagnosticNumberMismatchTest = testGroup "Number of Argument Mismatch"
    [ testCase "Concat Operation given 1" $ runExcept (runStateT (eval $ OperatorExp "concat" (VarExp "x") Nothing Nothing) envSingleVar) @?= Left (NumOfArgumentsError "Concat" 2 1 [])
    , testCase "Concat Operation given 3" $ runExcept (runStateT (eval $ OperatorExp "concat" (VarExp "x") (Just $ VarExp "y") (Just $ VarExp "z")) envSingleVar) @?= Left (NumOfArgumentsError "Concat" 2 3 [])
    , testCase "Union Operation given 1" $ runExcept (runStateT (eval $ OperatorExp "union" regexAorB Nothing Nothing) envSingleVar) @?= Left (NumOfArgumentsError "Union" 2 1 [])
    , testCase "Union Operation given 3" $ runExcept (runStateT (eval $ OperatorExp "union" regexAorB (Just regexAnyChar) (Just regexNotB)) envSingleVar) @?= Left (NumOfArgumentsError "Union" 2 3 [])
    , testCase "Unify Operation given 1" $ runExcept (runStateT (eval $ OperatorExp "unify" regexAorB Nothing Nothing) envSingleVar) @?= Left (NumOfArgumentsError "Unify" 2 1 [])
    , testCase "Unify Operation given 3" $ runExcept (runStateT (eval $ OperatorExp "unify" regexAorB (Just regexAnyChar) (Just regexNotB)) envSingleVar) @?= Left (NumOfArgumentsError "Unify" 2 3 [])
    , testCase "Subset Operation given 1" $ runExcept (runStateT (eval $ OperatorExp "subset" regexAorB Nothing Nothing) envSingleVar) @?= Left (NumOfArgumentsError "Subset" 2 1 [])
    , testCase "Subset Operation given 3" $ runExcept (runStateT (eval $ OperatorExp "subset" regexAorB (Just regexAnyChar) (Just regexNotB)) envSingleVar) @?= Left (NumOfArgumentsError "Subset" 2 3 [])
    , testCase "Singleton Operation given 2" $ runExcept (runStateT (eval $ OperatorExp "singleton" regexAorB (Just regexAnyChar) Nothing) envSingleVar) @?= Left (NumOfArgumentsError "Singleton" 1 2 [])
    , testCase "Singleton Operation given 3" $ runExcept (runStateT (eval $ OperatorExp "singleton" regexAorB (Just regexAnyChar) (Just regexNotB)) envSingleVar) @?= Left (NumOfArgumentsError "Singleton" 1 3 [])
    ]