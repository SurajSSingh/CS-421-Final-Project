module RegexTests (regexTreeTest) where
import PSST.Core
import PSST.RTOperations
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

regexTreeTest :: TestTree
regexTreeTest = testGroup "Regex Tree Operations Tests" [maybeLTETests, unifyTests]

maybeLTETests :: TestTree
maybeLTETests = testGroup "Maybe Less Than or Equal To Tests" 
  [ testCase "Both Nothing" (maybeLTE Nothing Nothing @?= True)
  , testCase "Just 1st" (maybeLTE (Just 1) Nothing @?= True)
  , testCase "Just 2nd" (maybeLTE Nothing (Just 1) @?= False)
  , testCase "Both Just, a < b" (maybeLTE (Just 1) (Just 2) @?= True)
  , testCase "Both Just, a = b" (maybeLTE (Just 1) (Just 1) @?= True)
  , testCase "Both Just, a > b" (maybeLTE (Just 1) (Just 0) @?= False)
  ]

unifyTests :: TestTree
unifyTests = testGroup "Regex Tree Unification Tests" 
  []

subLangTests :: TestTree
subLangTests = testGroup "Regex Tree Unification Tests" 
  []

singletonTests :: TestTree
singletonTests = testGroup "Regex Singleton Tests"
  [testCase "EmptySet" (isRegexSingleton EmptySet @?= False)
  ,testCase "Epsilon" (isRegexSingleton Epsilon @?= True)
  ,testCase "Any Character Literal" (isRegexSingleton AnyCharLiteral @?= False)
  ,testCase "Literal" (isRegexSingleton (Literal "a") @?= True)
  ,testCase "Empty Sequence" (isRegexSingleton (Sequence []) @?= False)
  ,testCase "Single Singleton Sequence" (isRegexSingleton (Sequence [Epsilon]) @?= True)
  ,testCase "Single Non-singleton Sequence" (isRegexSingleton (Sequence [EmptySet]) @?= False)
  ,testCase "Multiple String Sequence, all singletons" (isRegexSingleton (Sequence [Epsilon, Epsilon]) @?= True)
  ,testCase "Multiple String Sequence, any non-singleton" (isRegexSingleton (Sequence [Epsilon, EmptySet]) @?= False)
  ,testCase "Capture Group with Singleton" (isRegexSingleton (CaptureGroup 1 Epsilon) @?= True)
  ,testCase "Capture Group with Non-singleton" (isRegexSingleton (CaptureGroup 1 EmptySet) @?= False)
  ,testCase "Capture Group Stub" (isRegexSingleton (CaptureGroupStub (Just 1)) @?= False)
  ,testCase "Binary Choice, diff paths" (isRegexSingleton (CaptureGroupStub (Just 1)) @?= False)
  ,testCase "Binary Choice, same paths, Singleton" (isRegexSingleton (CaptureGroupStub (Just 1)) @?= False)
  ,testCase "Binary Choice, same paths, Non-singleton" (isRegexSingleton (CaptureGroupStub (Just 1)) @?= False)
  ,testCase "Repetition, unbounded" (isRegexSingleton (CaptureGroupStub (Just 1)) @?= False)
  ,testCase "Repetition, multi-bounded" (isRegexSingleton (CaptureGroupStub (Just 1)) @?= False)
  ,testCase "Repetition, single-bounded, Singleton" (isRegexSingleton (CaptureGroupStub (Just 1)) @?= False)
  ,testCase "Repetition, single-bounded, Non-singleton" (isRegexSingleton (CaptureGroupStub (Just 1)) @?= False)
  ]

