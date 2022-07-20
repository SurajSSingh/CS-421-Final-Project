module RegexTests (regexTreeTest) where
import PSST.Core
import PSST.RTOperations
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

simpleLiteral = Literal "a"
simpleSequence = Sequence [AnyCharLiteral, simpleLiteral]
simpleCaptureGroup tree = CaptureGroup 0 tree
simpleCaptureGroupStub = CaptureGroupStub Nothing
simpleStarRepetition tree = Repetition True 0 Nothing tree
simplePlusRepetition tree = Repetition True 0 Nothing tree
simpleOptRepetition tree = Repetition True 0 (Just 1) tree

regexTreeTest :: TestTree
regexTreeTest = testGroup "Regex Tree Operations Tests" [maybeLTETests, updateCaptureGroupNumberTests, unifyTests,subLangTests, singletonTests]

maybeLTETests :: TestTree
maybeLTETests = testGroup "Maybe Less Than or Equal To Tests" 
  [ testCase "Both Nothing" (maybeLTE Nothing Nothing @?= True)
  , testCase "Just 1st" (maybeLTE (Just 1) Nothing @?= True)
  , testCase "Just 2nd" (maybeLTE Nothing (Just 1) @?= False)
  , testCase "Both Just, a < b" (maybeLTE (Just 1) (Just 2) @?= True)
  , testCase "Both Just, a = b" (maybeLTE (Just 1) (Just 1) @?= True)
  , testCase "Both Just, a > b" (maybeLTE (Just 1) (Just 0) @?= False)
  ]

updateCaptureGroupNumberTests :: TestTree
updateCaptureGroupNumberTests = testGroup "Updating Capture Group Number Tests"
  []

unifyTests :: TestTree
unifyTests = testGroup "Regex Tree Unification Tests" 
  []

subLangTests :: TestTree
subLangTests = testGroup "Regex Tree Sub Language Tests" 
  [ emptySetSubLangTests
  , epsilonSubLangTests
  , literalSubLangTest
  ]

emptySetSubLangTests = testGroup "Empty Set subset of some Tree" 
  [ testCase "EmptySet/EmptySet" (isRegexSubLang EmptySet EmptySet @?= True)
  , testCase "EmptySet/Epsilon" (isRegexSubLang EmptySet Epsilon @?= True)
  , testCase "EmptySet/AnyCharLiteral" (isRegexSubLang EmptySet AnyCharLiteral @?= True)
  , testCase "EmptySet/Literal" (isRegexSubLang EmptySet simpleLiteral @?= True)
  , testCase "EmptySet/CaptureGroup" (isRegexSubLang EmptySet (simpleCaptureGroup simpleSequence) @?= True)
  , testCase "EmptySet/CaptureGroupStub" (isRegexSubLang EmptySet simpleCaptureGroupStub @?= True)
  , testCase "EmptySet/Sequence" (isRegexSubLang EmptySet simpleSequence @?= True)
  , testCase "EmptySet/BinChoice" (isRegexSubLang EmptySet (BinChoice simpleSequence AnyCharLiteral) @?= True)
  , testCase "EmptySet/Repetition" (isRegexSubLang EmptySet (simpleStarRepetition simpleSequence)  @?= True)
  ]

epsilonSubLangTests = testGroup "Epsilon subset of some Tree" 
  [ testCase "Epsilon/Epsilon" (isRegexSubLang Epsilon Epsilon @?= True)
  , testCase "Epsilon/AnyCharLiteral" (isRegexSubLang Epsilon AnyCharLiteral @?= False)
  , testCase "Epsilon/Literal" (isRegexSubLang Epsilon simpleLiteral @?= False)
  , testCase "Epsilon/CaptureGroup with Epsilon" (isRegexSubLang Epsilon (simpleCaptureGroup Epsilon) @?= True)
  , testCase "Epsilon/CaptureGroup without Epsilon" (isRegexSubLang Epsilon (simpleCaptureGroup AnyCharLiteral) @?= False)
  , testCase "Epsilon/CaptureGroupStub" (isRegexSubLang Epsilon simpleCaptureGroupStub @?= False)
  , testCase "Epsilon/Sequence, Empty" (isRegexSubLang Epsilon (Sequence []) @?= True)
  , testCase "Epsilon/Sequence, Single Epsilon" (isRegexSubLang Epsilon (Sequence [Epsilon]) @?= True)
  , testCase "Epsilon/Sequence, Multiple Epsilon" (isRegexSubLang Epsilon (Sequence [Epsilon, Epsilon]) @?= True)
  , testCase "Epsilon/Sequence, Anything" (isRegexSubLang Epsilon (Sequence [Epsilon, AnyCharLiteral]) @?= False)
  , testCase "Epsilon/BinChoice, Both Epsilon" (isRegexSubLang Epsilon (BinChoice Epsilon Epsilon) @?= True)
  , testCase "Epsilon/BinChoice, Left Epsilon" (isRegexSubLang Epsilon (BinChoice Epsilon AnyCharLiteral) @?= True)
  , testCase "Epsilon/BinChoice, Right Epsilon" (isRegexSubLang Epsilon (BinChoice AnyCharLiteral Epsilon) @?= True)
  , testCase "Epsilon/BinChoice, Other" (isRegexSubLang Epsilon (BinChoice AnyCharLiteral AnyCharLiteral) @?= False)
  , testCase "Epsilon/Repetition, Zero allowed" (isRegexSubLang Epsilon (simpleStarRepetition AnyCharLiteral) @?= True)
  , testCase "Epsilon/Repetition, More than zero, Epsilon" (isRegexSubLang Epsilon (simplePlusRepetition Epsilon) @?= False)
  , testCase "Epsilon/Repetition, More than zero no Epsilon" (isRegexSubLang Epsilon (simplePlusRepetition AnyCharLiteral) @?= False)
  ]

literalSubLangTest = testGroup "Literal subset of some Tree"
  [ testCase "Literal/EmptySet" (isRegexSubLang simpleLiteral EmptySet @?= False)
  , testCase "Literal/Epsilon" (isRegexSubLang simpleLiteral Epsilon @?= False)
  , testCase "Literal/Same Literal" (isRegexSubLang simpleLiteral simpleLiteral @?= True)
  , testCase "Literal/Different Literal" (isRegexSubLang simpleLiteral (Literal "b") @?= False)
  , testCase "Literal/Any Char" (isRegexSubLang simpleLiteral AnyCharLiteral @?= True)
  , testCase "Literal/Capture Group, with Same Literal" (isRegexSubLang simpleLiteral (simpleCaptureGroup simpleLiteral) @?= True)
  , testCase "Literal/Capture Group, not matching" (isRegexSubLang simpleLiteral (simpleCaptureGroup AnyCharLiteral) @?= False)
  , testCase "Literal/Capture Group Stub" (isRegexSubLang simpleLiteral simpleCaptureGroupStub @?= False)
  , testCase "Literal/Empty Sequence" (isRegexSubLang simpleLiteral (Sequence []) @?= False)
  , testCase "Literal/Sequence with exact literal" (isRegexSubLang simpleLiteral (Sequence [simpleLiteral]) @?= True)
  , testCase "Literal/Sequence with extra items" (isRegexSubLang simpleLiteral simpleSequence @?= False)
  , testCase "Literal/BinChoice, Both" (isRegexSubLang simpleLiteral (BinChoice simpleLiteral simpleLiteral) @?= True)
  , testCase "Literal/BinChoice, Left" (isRegexSubLang simpleLiteral (BinChoice simpleLiteral Epsilon) @?= True)
  , testCase "Literal/BinChoice, Right" (isRegexSubLang simpleLiteral (BinChoice Epsilon simpleLiteral) @?= True)
  , testCase "Literal/BinChoice, None" (isRegexSubLang simpleLiteral (BinChoice Epsilon Epsilon) @?= False)
  , testCase "Literal/Repetition, 0+" (isRegexSubLang simpleLiteral (simpleStarRepetition simpleLiteral) @?= True)
  , testCase "Literal/Repetition, 2+" (isRegexSubLang simpleLiteral (Repetition False 2 Nothing simpleLiteral) @?= False)
  ]

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
  ,testCase "Binary Choice, diff paths" (isRegexSingleton (BinChoice EmptySet Epsilon) @?= False)
  ,testCase "Binary Choice, same paths, Singleton" (isRegexSingleton (BinChoice Epsilon Epsilon) @?= True)
  ,testCase "Binary Choice, same paths, Non-singleton" (isRegexSingleton (BinChoice EmptySet EmptySet) @?= False)
  ,testCase "Repetition, unbounded" (isRegexSingleton (Repetition False 0 Nothing Epsilon) @?= False)
  ,testCase "Repetition, multi-bounded" (isRegexSingleton (Repetition False 0 (Just 1) Epsilon) @?= False)
  ,testCase "Repetition, single-bounded, Singleton" (isRegexSingleton (Repetition False 1 (Just 1) Epsilon) @?= True)
  ,testCase "Repetition, single-bounded, Non-singleton" (isRegexSingleton (Repetition False 1 (Just 1) EmptySet) @?= False)
  ]

