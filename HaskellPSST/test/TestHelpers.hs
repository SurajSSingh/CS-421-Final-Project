module TestHelpers where

import PSST.Core
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- regexTestHelper :: String -> RegexTree -> TestTree
-- regexTestHelper regexStr regexTree = testCase ("Parsing " ++ regexStr) (strSolParseR ("\"" ++ regexStr ++ "\"") @?= Right regexTree)