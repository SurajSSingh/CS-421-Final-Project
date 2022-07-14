import PSST.Transducer
import Test.Tasty
import Test.Tasty.HUnit
import PSST.Parser
import PSST.Core

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parserTests, evaluatorTests]

parserTests :: TestTree
parserTests = testGroup "Parser Tests" [parseExpTest, parseDiagnosticTest]

parseExpTest :: TestTree
parseExpTest = testGroup "Parse Expression Tests" 
  [ testGroup "Parse Values Tests" 
    [ testGroup "Parse Integer" 
      [ testCase "Parse 0"  (strSolParse "0" @?= Right (ValExp (IntVal 0)))
      , testCase "Parse 1"  (strSolParse "1" @?= Right (ValExp (IntVal 1)))
      , testCase "Parse 100"  (strSolParse "100" @?= Right (ValExp (IntVal 100)))
      , testCase "Parse 789"  (strSolParse "789" @?= Right (ValExp (IntVal 789)))
      ]
    , testGroup "Parse String/Regex" 
      [ testCase "Parse Simple String"  (strSolParse "Hello" @?= Right (ValExp (RegexVal "Hello")))
      , testCase "Parse Regex"  (strSolParse "\\d+" @?= Right (ValExp (RegexVal "\\d+")))
      , testCase "Parse Number String"  (strSolParse "100" @?= Right (ValExp (RegexVal "100")))
      , testCase "Parse Complex Regex"  (strSolParse "([abc]+[^abc])*" @?= Right (ValExp (RegexVal "([abc]+[^abc])*")))
      ]
    ]
  ]

parseDiagnosticTest :: TestTree
parseDiagnosticTest = testGroup "Diagnostic Tests" []


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

