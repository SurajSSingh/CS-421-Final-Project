import PSST.Transducer
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec.Error (ParseError)
import PSST.Parser
import PSST.Core

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [simpleMathTests, parserTests, evaluatorTests]

parserTests :: TestTree
parserTests = testGroup "Parser Tests" [parseExpTest, parseErrorTest]

parseExpTest :: TestTree
parseExpTest = testGroup "Parse Expression Tests"
  [ testGroup "Parse Values Tests"
    [ testGroup "Parse Integer"
      [ testCase "Parse 0"  (strSolParse "0" @?= Right (ValExp (IntVal 0)))
      , testCase "Parse 1"  (strSolParse "1" @?= Right (ValExp (IntVal 1)))
      , testCase "Parse 123"  (strSolParse "123" @?= Right (ValExp (IntVal 123)))
      , testCase "Parse Leading Spaces"  (strSolParse "    456" @?= Right (ValExp (IntVal 456)))
      , testCase "Parse Trailing Spaces"  (strSolParse "789    " @?= Right (ValExp (IntVal 789)))
      , testCase "Parse Spaces Around"  (strSolParse "   314   " @?= Right (ValExp (IntVal 314)))
      ]
      , testGroup "Parse String/Regex"
      [ testCase "Parse Epsilon"  (strSolParse "``" @?= Right (ValExp (RegexVal "")))
      , testCase "Parse Single Literal"  (strSolParse "`a`" @?= Right (ValExp (RegexVal "a")))
      , testCase "Parse Sequence of Literal"  (strSolParse "`abc`" @?= Right (ValExp (RegexVal "abc")))
      , testCase "Parse Simple Regex"  (strSolParse "`[abc]`" @?= Right (ValExp (RegexVal "[abc]")))
      -- , testCase "Parse Complex Regex"  (strSolParse "`(a|b)*([^cd]?[efg])+`" @?= Right (ValExp (RegexVal "\\d+")))
      -- , testCase "Parse Lazy Complex Regex"  (strSolParse "`(a|b)*?[^cd]??[efg]*?`" @?= Right (ValExp (RegexVal "\\d+")))
      -- [ testCase "Parse Epsilon"  (strSolParse "``" @?= Right (ValExp (RegexVal Epsilon)))
      -- , testCase "Parse Single Literal"  (strSolParse "`a`" @?= Right (ValExp (RegexVal (Literal 'a'))))
      -- , testCase "Parse Sequence of Literal"  (strSolParse "`abc`" @?= Right (ValExp (RegexVal (Sequence [(Literal 'a'), (Literal 'b'), (Literal 'c')]))))
      -- , testCase "Parse Number String"  (strSolParse "\"100\"" @?= Right (ValExp (RegexVal "100")))
      -- , testCase "Parse Complex Regex"  (strSolParse "\"([abc]+[^abc])*\"" @?= Right (ValExp (RegexVal "([abc]+[^abc])*")))
      ]
    ]
  , testGroup "Parse Variable"
    [ testCase "Alphabet-only variable name"  (strSolParse "x" @?= Right (VarExp "x"))
    , testCase "AlphaNumeric variable name"  (strSolParse "x10" @?= Right (VarExp "x10"))
    , testCase "AlphaNumeric with Underscore variable name"  (strSolParse "x_10" @?= Right (VarExp "x_10"))
    ]
  , testGroup "Parse Assignment"
    [ testCase "Parse Assign Number" (strSolParse "x = 10" @?= Right (AssignmentExp "x" (ValExp (IntVal 10))))
    , testCase "Parse Assign String" (strSolParse "x = `hello`" @?= Right (AssignmentExp "x" (ValExp (IntVal 10))))
    ]
  , testGroup "Parse Operators"
    []
  ]

-- TODO: Figure out how to add test for Errors
parseErrorTest :: TestTree
parseErrorTest = testGroup "Parse Error Tests"
  [ testGroup "Value Parse Error"
    [ -- testCase "Parse Negative Number" $ strSolParse "-1" @?= Left (ParseError "Error: ")

    ]

  ]


evaluatorTests :: TestTree
evaluatorTests = testGroup "Evaluator Tests" []


simpleMathFunction :: Int -> Int -> Int -> Int
simpleMathFunction a b c = a * b - c

simpleMathTests :: TestTree
simpleMathTests = testGroup "Simple Math Tests"
  [ testCase "Small Numbers" $
      simpleMathFunction 3 4 5 @?= 7,
    testCase "Medium Numbers" $
        simpleMathFunction 10 20 30 @?= 170
  ]

