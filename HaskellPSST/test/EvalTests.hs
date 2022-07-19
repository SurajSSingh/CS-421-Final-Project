module EvalTests (evaluatorTests) where

import PSST.Core
import PSST.Evaluator
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
    
evaluatorTests :: TestTree
evaluatorTests = testGroup "Evaluator Tests" []