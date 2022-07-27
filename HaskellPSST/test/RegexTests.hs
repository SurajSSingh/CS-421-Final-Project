module RegexTests (regexTreeTest) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)
import PSST.Parser (strSolParseRegex)
import qualified Text.Parsec as Text.Parsec.Error
import PSST.Core (RegexNode (CaptureGroupSequence), emptySet)
import PSST.RTOperations (isNodeSingleton, regexUnify, regexUnion)

regexNodeGenerator :: String -> RegexNode
regexNodeGenerator regexStr = case strSolParseRegex ("\"" ++ regexStr ++ "\"") of
  Left pe -> CaptureGroupSequence 0 []
  Right rn -> rn

singletonTestHelper :: String -> Bool -> TestTree
singletonTestHelper s b = testCase ("Singleton: " ++ s) (isNodeSingleton (regexNodeGenerator s) @?= b)

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
    [singletonTests, regexUnionTests, regexUnifyTests]

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
regexSubNodeTests = testGroup "Regex Sub-Nodes Tests" []


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

-- [maybeLTETests, updateCaptureGroupNumberTests, regexTreeUnion, singletonTests, subLangTests, unifyTests]

-- maybeLTETests :: TestTree
-- maybeLTETests = testGroup "Maybe Less Than or Equal To Tests" 
--   [ testCase "Both Nothing" (maybeLTE Nothing Nothing @?= True)
--   , testCase "Just 1st" (maybeLTE (Just 1) Nothing @?= True)
--   , testCase "Just 2nd" (maybeLTE Nothing (Just 1) @?= False)
--   , testCase "Both Just, a < b" (maybeLTE (Just 1) (Just 2) @?= True)
--   , testCase "Both Just, a = b" (maybeLTE (Just 1) (Just 1) @?= True)
--   , testCase "Both Just, a > b" (maybeLTE (Just 1) (Just 0) @?= False)
--   ]

-- updateCaptureGroupNumberTests :: TestTree
-- updateCaptureGroupNumberTests = testGroup "Updating Capture Group Number Tests"
--   []

-- unifyTests :: TestTree
-- unifyTests = testGroup "Regex Tree Unification Tests" 
--   []

-- unionTests :: TestTree
-- unionTests = testGroup "Regex Tree Union Tests" 
--   []

-- subLangTests :: TestTree
-- subLangTests = testGroup "Regex Tree Sub Language Tests" 
--   [ emptySetSubLangTests
--   , epsilonSubLangTests
--   , literalSubLangTests
--   , anyCharSubLangTests
--   , captureGroupSubLangTests
--   , captureGroupStubSubLangTests
--   ]

-- emptySetSubLangTests = testGroup "Empty Set subset of some Tree" 
--   [ testCase "EmptySet/EmptySet" (isRegexSubLang EmptySet EmptySet @?= True)
--   , testCase "EmptySet/Epsilon" (isRegexSubLang EmptySet Epsilon @?= True)
--   , testCase "EmptySet/AnyCharLiteral" (isRegexSubLang EmptySet AnyCharLiteral @?= True)
--   , testCase "EmptySet/Literal" (isRegexSubLang EmptySet simpleLiteral @?= True)
--   , testCase "EmptySet/CaptureGroup" (isRegexSubLang EmptySet (simpleCaptureGroup simpleSequence) @?= True)
--   , testCase "EmptySet/CaptureGroupStub" (isRegexSubLang EmptySet simpleCaptureGroupStub @?= True)
--   , testCase "EmptySet/Sequence" (isRegexSubLang EmptySet simpleSequence @?= True)
--   , testCase "EmptySet/BinChoice" (isRegexSubLang EmptySet (BinChoice simpleSequence AnyCharLiteral) @?= True)
--   , testCase "EmptySet/Repetition" (isRegexSubLang EmptySet (simpleStarRepetition simpleSequence)  @?= True)
--   ]

-- epsilonSubLangTests = testGroup "Epsilon subset of some Tree" 
--   [ testCase "Epsilon/Epsilon" (isRegexSubLang Epsilon Epsilon @?= True)
--   , testCase "Epsilon/AnyCharLiteral" (isRegexSubLang Epsilon AnyCharLiteral @?= False)
--   , testCase "Epsilon/Literal" (isRegexSubLang Epsilon simpleLiteral @?= False)
--   , testCase "Epsilon/CaptureGroup with Epsilon" (isRegexSubLang Epsilon (simpleCaptureGroup Epsilon) @?= True)
--   , testCase "Epsilon/CaptureGroup without Epsilon" (isRegexSubLang Epsilon (simpleCaptureGroup AnyCharLiteral) @?= False)
--   , testCase "Epsilon/CaptureGroupStub" (isRegexSubLang Epsilon simpleCaptureGroupStub @?= False)
--   , testCase "Epsilon/Sequence, Empty" (isRegexSubLang Epsilon (Sequence []) @?= True)
--   , testCase "Epsilon/Sequence, Single Epsilon" (isRegexSubLang Epsilon (Sequence [Epsilon]) @?= True)
--   , testCase "Epsilon/Sequence, Multiple Epsilon" (isRegexSubLang Epsilon (Sequence [Epsilon, Epsilon]) @?= True)
--   , testCase "Epsilon/Sequence, Anything" (isRegexSubLang Epsilon (Sequence [Epsilon, AnyCharLiteral]) @?= False)
--   , testCase "Epsilon/BinChoice, Both Epsilon" (isRegexSubLang Epsilon (BinChoice Epsilon Epsilon) @?= True)
--   , testCase "Epsilon/BinChoice, Left Epsilon" (isRegexSubLang Epsilon (BinChoice Epsilon AnyCharLiteral) @?= True)
--   , testCase "Epsilon/BinChoice, Right Epsilon" (isRegexSubLang Epsilon (BinChoice AnyCharLiteral Epsilon) @?= True)
--   , testCase "Epsilon/BinChoice, Other" (isRegexSubLang Epsilon (BinChoice AnyCharLiteral AnyCharLiteral) @?= False)
--   , testCase "Epsilon/Repetition, Zero allowed" (isRegexSubLang Epsilon (simpleStarRepetition AnyCharLiteral) @?= True)
--   , testCase "Epsilon/Repetition, More than zero, Epsilon" (isRegexSubLang Epsilon (simplePlusRepetition Epsilon) @?= False)
--   , testCase "Epsilon/Repetition, More than zero no Epsilon" (isRegexSubLang Epsilon (simplePlusRepetition AnyCharLiteral) @?= False)
--   ]

-- literalSubLangTests = testGroup "Literal subset of some Tree"
--   [ testCase "Literal/EmptySet" (isRegexSubLang simpleLiteral EmptySet @?= False)
--   , testCase "Literal/Epsilon" (isRegexSubLang simpleLiteral Epsilon @?= False)
--   , testCase "Literal/Same Literal" (isRegexSubLang simpleLiteral simpleLiteral @?= True)
--   , testCase "Literal/Different Literal" (isRegexSubLang simpleLiteral (Literal "b") @?= False)
--   , testCase "Literal/Any Char" (isRegexSubLang simpleLiteral AnyCharLiteral @?= True)
--   , testCase "Literal/Capture Group, with Same Literal" (isRegexSubLang simpleLiteral (simpleCaptureGroup simpleLiteral) @?= True)
--   , testCase "Literal/Capture Group, not matching" (isRegexSubLang simpleLiteral (simpleCaptureGroup AnyCharLiteral) @?= False)
--   , testCase "Literal/Capture Group Stub" (isRegexSubLang simpleLiteral simpleCaptureGroupStub @?= False)
--   , testCase "Literal/Empty Sequence" (isRegexSubLang simpleLiteral (Sequence []) @?= False)
--   , testCase "Literal/Sequence with exact literal" (isRegexSubLang simpleLiteral (Sequence [simpleLiteral]) @?= True)
--   , testCase "Literal/Sequence with extra items" (isRegexSubLang simpleLiteral simpleSequence @?= False)
--   , testCase "Literal/BinChoice, Both" (isRegexSubLang simpleLiteral (BinChoice simpleLiteral simpleLiteral) @?= True)
--   , testCase "Literal/BinChoice, Left" (isRegexSubLang simpleLiteral (BinChoice simpleLiteral Epsilon) @?= True)
--   , testCase "Literal/BinChoice, Right" (isRegexSubLang simpleLiteral (BinChoice Epsilon simpleLiteral) @?= True)
--   , testCase "Literal/BinChoice, None" (isRegexSubLang simpleLiteral (BinChoice Epsilon Epsilon) @?= False)
--   , testCase "Literal/Repetition, 0+" (isRegexSubLang simpleLiteral (simpleStarRepetition simpleLiteral) @?= True)
--   , testCase "Literal/Repetition, 2+" (isRegexSubLang simpleLiteral (Repetition False 2 Nothing simpleLiteral) @?= False)
--   ]

-- anyCharSubLangTests = testGroup "Any Char Literal subset of some Tree"
--   [ testCase "AnyCharLiteral/EmptySet" (isRegexSubLang AnyCharLiteral EmptySet @?= False)
--   , testCase "AnyCharLiteral/Epsilon" (isRegexSubLang AnyCharLiteral Epsilon @?= False)
--   , testCase "AnyCharLiteral/AnyCharLiteral" (isRegexSubLang AnyCharLiteral AnyCharLiteral @?= True)
--   , testCase "AnyCharLiteral/Literal" (isRegexSubLang AnyCharLiteral simpleLiteral @?= False)
--   , testCase "AnyCharLiteral/CaptureGroup with AnyCharLiteral" (isRegexSubLang AnyCharLiteral (simpleCaptureGroup AnyCharLiteral) @?= True)
--   , testCase "AnyCharLiteral/CaptureGroup, not AnyCharLiteral" (isRegexSubLang AnyCharLiteral (simpleCaptureGroup Epsilon) @?= False)
--   , testCase "AnyCharLiteral/CaptureGroupStub" (isRegexSubLang AnyCharLiteral simpleCaptureGroupStub @?= False)
--   , testCase "AnyCharLiteral/Sequence, Empty" (isRegexSubLang AnyCharLiteral (Sequence []) @?= False)
--   , testCase "AnyCharLiteral/Sequence, Single with" (isRegexSubLang AnyCharLiteral (Sequence [AnyCharLiteral]) @?= False)
--   , testCase "AnyCharLiteral/Sequence, Single without" (isRegexSubLang AnyCharLiteral (Sequence [simpleLiteral]) @?= False)
--   , testCase "AnyCharLiteral/Sequence, Multiple" (isRegexSubLang AnyCharLiteral (Sequence [simpleLiteral, AnyCharLiteral]) @?= False)
--   , testCase "AnyCharLiteral/Choice, Both" (isRegexSubLang AnyCharLiteral (BinChoice AnyCharLiteral AnyCharLiteral) @?= True)
--   , testCase "AnyCharLiteral/Choice, Left" (isRegexSubLang AnyCharLiteral (BinChoice AnyCharLiteral Epsilon) @?= True)
--   , testCase "AnyCharLiteral/Choice, Right" (isRegexSubLang AnyCharLiteral (BinChoice Epsilon AnyCharLiteral) @?= True)
--   , testCase "AnyCharLiteral/Choice, None" (isRegexSubLang AnyCharLiteral (BinChoice Epsilon simpleLiteral) @?= False)
--   , testCase "AnyCharLiteral/Repetition 0+" (isRegexSubLang AnyCharLiteral (simpleStarRepetition AnyCharLiteral) @?= True)
--   , testCase "AnyCharLiteral/Epsilon" (isRegexSubLang AnyCharLiteral Epsilon @?= False)
--   ]

-- captureGroupSubLangTests = testGroup "Capture Group subset of some Tree"
--   [ testCase "CaptureGroup/EmptySet, has subset" (isRegexSubLang (simpleCaptureGroup EmptySet) EmptySet @?= True)
--   , testCase "CaptureGroup/EmptySet, doesn't have subset" (isRegexSubLang (simpleCaptureGroup AnyCharLiteral) EmptySet @?= False)
--   , testCase "CaptureGroup/Epsilon, has subset" (isRegexSubLang (simpleCaptureGroup Epsilon) Epsilon @?= True)
--   , testCase "CaptureGroup/Epsilon, doesn't have subset" (isRegexSubLang (simpleCaptureGroup Epsilon) AnyCharLiteral @?= False)
--   , testCase "CaptureGroup/AnyCharLiter, has subset" (isRegexSubLang (simpleCaptureGroup Epsilon) Epsilon @?= True)
--   , testCase "CaptureGroup/AnyCharLiter, doesn't have subset" (isRegexSubLang (simpleCaptureGroup Epsilon) AnyCharLiteral @?= False)
--   , testCase "CaptureGroup/Literal, has subset" (isRegexSubLang (simpleCaptureGroup Epsilon) Epsilon @?= True)
--   , testCase "CaptureGroup/Literal, doesn't have subset" (isRegexSubLang (simpleCaptureGroup Epsilon) AnyCharLiteral @?= False)
--   , testCase "CaptureGroup/CaptureGroup, has subset" (isRegexSubLang (simpleCaptureGroup Epsilon) (CaptureGroup 100 Epsilon) @?= True)
--   , testCase "CaptureGroup/CaptureGroup, doesn't have subset" (isRegexSubLang (simpleCaptureGroup Epsilon) (CaptureGroup 100  AnyCharLiteral) @?= False)
--   , testCase "CaptureGroup/CaptureGroupStub" (isRegexSubLang (simpleCaptureGroup Epsilon) simpleCaptureGroupStub @?= False)
--   , testCase "CaptureGroup/Sequence, Empty" (isRegexSubLang (simpleCaptureGroup Epsilon) (Sequence []) @?= True)
--   , testCase "CaptureGroup/Sequence, Single, has subset" (isRegexSubLang (simpleCaptureGroup Epsilon) (Sequence [Epsilon]) @?= True)
--   , testCase "CaptureGroup/Sequence, Single, doesn't have subset" (isRegexSubLang (simpleCaptureGroup Epsilon) (Sequence [AnyCharLiteral]) @?= False)
--   , testCase "CaptureGroup/Sequence, Multiple" (isRegexSubLang (simpleCaptureGroup Epsilon) (Sequence [AnyCharLiteral]) @?= False)
--   , testCase "CaptureGroup/BinChoice, has subset" (isRegexSubLang (simpleCaptureGroup Epsilon) Epsilon @?= True)
--   , testCase "CaptureGroup/BinChoice, doesn't have subset" (isRegexSubLang (simpleCaptureGroup Epsilon) AnyCharLiteral @?= False)
--   , testCase "CaptureGroup/BinChoice, has subset" (isRegexSubLang (simpleCaptureGroup Epsilon) Epsilon @?= True)
--   , testCase "CaptureGroup/BinChoice, doesn't have subset" (isRegexSubLang (simpleCaptureGroup Epsilon) AnyCharLiteral @?= False)
--   ]

-- captureGroupStubSubLangTests = testGroup "Capture Group subset of some Tree"
--   [ testCase "CaptureGroupStub/EmptySet" (isRegexSubLang simpleCaptureGroupStub EmptySet @?= False)
--   , testCase "CaptureGroupStub/Epsilon" (isRegexSubLang simpleCaptureGroupStub Epsilon @?= False)
--   , testCase "CaptureGroupStub/AnyCharLiteral" (isRegexSubLang simpleCaptureGroupStub AnyCharLiteral @?= False)
--   , testCase "CaptureGroupStub/Literal" (isRegexSubLang simpleCaptureGroupStub simpleLiteral @?= False)
--   , testCase "CaptureGroupStub/CaptureGroup" (isRegexSubLang simpleCaptureGroupStub EmptySet @?= False)
--   , testCase "CaptureGroupStub/CaptureGroupStub, same" (isRegexSubLang simpleCaptureGroupStub (CaptureGroupStub Nothing) @?= True)
--   , testCase "CaptureGroupStub/CaptureGroupStub, diff" (isRegexSubLang simpleCaptureGroupStub (CaptureGroupStub $ Just 0) @?= False)
--   , testCase "CaptureGroupStub/Sequence, Empty" (isRegexSubLang simpleCaptureGroupStub (Sequence []) @?= False)
--   , testCase "CaptureGroupStub/Sequence, Single, same" (isRegexSubLang simpleCaptureGroupStub (Sequence [simpleCaptureGroupStub]) @?= True)
--   , testCase "CaptureGroupStub/Sequence, Single, diff" (isRegexSubLang simpleCaptureGroupStub (Sequence [(CaptureGroupStub $ Just 0)]) @?= False)
--   , testCase "CaptureGroupStub/Sequence, Multiple" (isRegexSubLang simpleCaptureGroupStub (Sequence [simpleCaptureGroupStub, AnyCharLiteral]) @?= False)
--   , testCase "CaptureGroupStub/BinChoice, Both" (isRegexSubLang simpleCaptureGroupStub (BinChoice simpleCaptureGroupStub simpleCaptureGroupStub) @?= True)
--   , testCase "CaptureGroupStub/BinChoice, Left" (isRegexSubLang simpleCaptureGroupStub (BinChoice simpleCaptureGroupStub Epsilon) @?= True)
--   , testCase "CaptureGroupStub/BinChoice, Right" (isRegexSubLang simpleCaptureGroupStub (BinChoice Epsilon simpleCaptureGroupStub) @?= True)
--   , testCase "CaptureGroupStub/BinChoice, None" (isRegexSubLang simpleCaptureGroupStub (BinChoice Epsilon AnyCharLiteral) @?= False)
--   , testCase "CaptureGroupStub/Repetition 0+" (isRegexSubLang simpleCaptureGroupStub (simpleStarRepetition simpleCaptureGroupStub) @?= True)
--   ]

-- singletonTests :: TestTree
-- singletonTests = testGroup "Regex Singleton Tests"
--   [testCase "EmptySet" (isRegexSingleton EmptySet @?= False)
--   ,testCase "Epsilon" (isRegexSingleton Epsilon @?= True)
--   ,testCase "Any Character Literal" (isRegexSingleton AnyCharLiteral @?= False)
--   ,testCase "Literal" (isRegexSingleton (Literal "a") @?= True)
--   ,testCase "Empty Sequence" (isRegexSingleton (Sequence []) @?= False)
--   ,testCase "Single Singleton Sequence" (isRegexSingleton (Sequence [Epsilon]) @?= True)
--   ,testCase "Single Non-singleton Sequence" (isRegexSingleton (Sequence [EmptySet]) @?= False)
--   ,testCase "Multiple String Sequence, all singletons" (isRegexSingleton (Sequence [Epsilon, Epsilon]) @?= True)
--   ,testCase "Multiple String Sequence, any non-singleton" (isRegexSingleton (Sequence [Epsilon, EmptySet]) @?= False)
--   ,testCase "Capture Group with Singleton" (isRegexSingleton (CaptureGroup 1 Epsilon) @?= True)
--   ,testCase "Capture Group with Non-singleton" (isRegexSingleton (CaptureGroup 1 EmptySet) @?= False)
--   ,testCase "Capture Group Stub" (isRegexSingleton (CaptureGroupStub (Just 1)) @?= False)
--   ,testCase "Binary Choice, diff paths" (isRegexSingleton (BinChoice EmptySet Epsilon) @?= False)
--   ,testCase "Binary Choice, same paths, Singleton" (isRegexSingleton (BinChoice Epsilon Epsilon) @?= True)
--   ,testCase "Binary Choice, same paths, Non-singleton" (isRegexSingleton (BinChoice EmptySet EmptySet) @?= False)
--   ,testCase "Repetition, unbounded" (isRegexSingleton (Repetition False 0 Nothing Epsilon) @?= False)
--   ,testCase "Repetition, multi-bounded" (isRegexSingleton (Repetition False 0 (Just 1) Epsilon) @?= False)
--   ,testCase "Repetition, single-bounded, Singleton" (isRegexSingleton (Repetition False 1 (Just 1) Epsilon) @?= True)
--   ,testCase "Repetition, single-bounded, Non-singleton" (isRegexSingleton (Repetition False 1 (Just 1) EmptySet) @?= False)
--   ]

