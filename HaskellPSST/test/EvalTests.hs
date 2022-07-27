module EvalTests (evaluatorTests) where

import PSST.Core
import PSST.Evaluator
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
    
showResult :: Show a1 => a1 -> Either Diagnostic Exp
showResult a = Right $ ResultValExp $ show a

evaluatorTests :: TestTree
evaluatorTests = testGroup "Evaluator Tests" []