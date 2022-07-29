module RegexTests (regexTreeTest) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)
import PSST.Parser (strSolParseRegex)
import qualified Text.Parsec as Text.Parsec.Error
import PSST.Core (RegexNode (CaptureGroupSequence), emptySet)
import PSST.RTOperations (isNodeSingleton, regexUnify, regexUnion, isSubNode)

regexNodeGenerator :: String -> RegexNode
regexNodeGenerator regexStr = case strSolParseRegex ("\"" ++ regexStr ++ "\"") of
  Left pe -> CaptureGroupSequence 0 []
  Right rn -> rn

singletonTestHelper :: String -> Bool -> TestTree
singletonTestHelper s b = testCase ("Singleton: " ++ s) (isNodeSingleton (regexNodeGenerator s) @?= b)

subnodeTestHelper :: String -> String -> Bool -> TestTree
subnodeTestHelper s1 s2 b = testCase ("Subset: " ++ s1 ++ " to " ++ s2) (isSubNode (regexNodeGenerator s1) (regexNodeGenerator s2) @?= b)

binaryRegexOpHelper :: (RegexNode -> RegexNode -> RegexNode) -> String -> String -> String -> Assertion
binaryRegexOpHelper f a b x = f (regexNodeGenerator a) (regexNodeGenerator b) @?= regexNodeGenerator x

unifyOpHelper :: String -> String -> Maybe String -> TestTree
unifyOpHelper a b x = testCase ("Unify: " ++ a ++ " & " ++  b) (case x of
   Nothing -> regexUnify (regexNodeGenerator a) (regexNodeGenerator b) @?= emptySet
   Just s -> binaryRegexOpHelper regexUnify a b s)

unionOpHelper :: String -> String -> String -> TestTree
unionOpHelper a b x = testCase ("Union: " ++ a ++ " | " ++  b) $ binaryRegexOpHelper regexUnion a b x

regexTreeTest :: TestTree
regexTreeTest = testGroup "Regex Tree Operations Tests"
    [singletonTests, regexSubNodeTests, regexUnionTests, regexUnifyTests]

singletonTests :: TestTree
singletonTests = testGroup "Singleton Tests"
    [ singletonTestHelper "" True
    , singletonTestHelper "." False
    , singletonTestHelper "a" True
    , singletonTestHelper "(a)" True
    , singletonTestHelper "((a)b)" True
    , singletonTestHelper "abc" True
    , singletonTestHelper "a|b" False
    , singletonTestHelper "a|a" True
    , singletonTestHelper "a?" False
    , singletonTestHelper "a*" False
    , singletonTestHelper "a+" False
    , singletonTestHelper "a{2}" True
    , singletonTestHelper "a{2,4}" False
    , singletonTestHelper "a{2,}" False
    ]

regexSubNodeTests :: TestTree
regexSubNodeTests = testGroup "Regex Sub-Nodes Tests" [regexSubNodeEqualityTests, regexSubNodeTrueTests, regexSubNodeFalseTests]

regexSubNodeEqualityTests :: TestTree
regexSubNodeEqualityTests = testGroup "Regex Equal value Sub-Nodes Tests"
    [ subnodeTestHelper "a" "a" True
    , subnodeTestHelper "" "" True
    , subnodeTestHelper "." "." True
    , subnodeTestHelper "(a)." "(a)." True
    , subnodeTestHelper "a*" "a*" True
    , subnodeTestHelper "a?" "a?" True
    , subnodeTestHelper "a+" "a+" True
    , subnodeTestHelper "a{1,2}" "a{1,2}" True
    , subnodeTestHelper "abc" "abc" True
    , subnodeTestHelper "abc" "(abc)" True
    ]

regexSubNodeTrueTests :: TestTree
regexSubNodeTrueTests = testGroup "Regex True Sub-Nodes Tests"
    [ subnodeTestHelper "a" "." True
    , subnodeTestHelper "" "a*" True
    , subnodeTestHelper "a" "a+" True
    , subnodeTestHelper "" "a?" True
    , subnodeTestHelper "a" "a?" True
    , subnodeTestHelper "a" "a|b" True
    , subnodeTestHelper "a" "(a*)|b" True
    , subnodeTestHelper "" "(a?)|b" True
    , subnodeTestHelper "abc" "(abc)|(xyz)" True
    , subnodeTestHelper "abc" "(abc)*" True
    , subnodeTestHelper "a{10,100}" "a*" True
    , subnodeTestHelper "a{10,100}" "a{5,500}" True
    , subnodeTestHelper "a{10,}" "a*" True
    , subnodeTestHelper "a{10,}" "a{5,}" True
    ]

regexSubNodeFalseTests :: TestTree
regexSubNodeFalseTests = testGroup "Regex Not Sub-Nodes Tests"
    [ subnodeTestHelper "." "a" False
    , subnodeTestHelper "" "a" False
    ]

regexUnionTests :: TestTree
regexUnionTests = testGroup "Regex Union Tests"
    [ unionOpHelper "a" "a" "a"
    , unionOpHelper "a" "b" "a|b"
    , unionOpHelper "a" "" "a?"
    , unionOpHelper "a|b" "c|d" "(a|b)|(c|d)"
    , unionOpHelper "a" "a?" "a?"
    , unionOpHelper "a" "a??" "a??"
    , unionOpHelper "a+" "a*" "a*"
    , unionOpHelper "a+" "a?" "a+"
    , unionOpHelper "a?" "a*" "a*"
    ]

regexUnifyTests :: TestTree
regexUnifyTests = testGroup "Regex Unify Tests"
    [ unifyOpHelper "a" "a" $ Just "a"
    , unifyOpHelper "a" "b" Nothing
    , unifyOpHelper "a" "a?" $ Just "a"
    , unifyOpHelper "a" "a??" $ Just "a"
    , unifyOpHelper "a+" "a*" $ Just "a+"
    , unifyOpHelper "a+" "a?" $ Just "a?"
    , unifyOpHelper "a?" "a*" $ Just "a?"
    , unifyOpHelper "a|b" "b|a" $ Just "a|b"
    ]
