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
parserTests = testGroup "Parser Tests" [parseStringToTree, parseExpTest, parseErrorTest]

regexTestHelper :: String -> RegexTree -> TestTree
regexTestHelper regexStr regexTree = testCase ("Parsing " ++ regexStr) (strSolParseR ("\"" ++ regexStr ++ "\"") @?= Right regexTree)

parseStringToTree :: TestTree
parseStringToTree = testGroup "Parse Regex String to Regex Tree"
  [ testGroup "Parsing Single Literal"
    [ regexTestHelper "a" (Literal "a")
    , regexTestHelper "1" (Literal "1")
    , regexTestHelper "." (AnyCharLiteral)
    ]
  , testGroup "Parsing Escaped Literal"
    [ regexTestHelper "\\\\" (Literal "\\\\")
    , regexTestHelper "\\n" (Literal "\\n")
    , regexTestHelper "\\t" (Literal "\\t")
    , regexTestHelper "\\*" (Literal "*")
    , regexTestHelper "\\$" (Literal "$")
    , regexTestHelper "\\." (Literal ".")
    ]
  , testGroup "Parsing Sequence of Literal"
    [ regexTestHelper "ab" (Sequence [Literal "a",Literal "b"])
    , regexTestHelper "abc" (Sequence [Literal "a",Literal "b", Literal "c"])
    , regexTestHelper "123" (Sequence [Literal "1",Literal "2", Literal "3"])
    ]
  , testGroup "Parsing Capture Group only"
    [ regexTestHelper "(a)" (CaptureGroup 1 (Literal "a"))
    , regexTestHelper "((a))" (CaptureGroup 1 (CaptureGroup 2 (Literal "a")))
    , regexTestHelper "(a)(b)" (Sequence [CaptureGroup 1 (Literal "a"), CaptureGroup 2 (Literal "b")])
    , regexTestHelper "((a)(b))" (CaptureGroup 1 (Sequence [CaptureGroup 2 (Literal "a"), CaptureGroup 3 (Literal "b")]))
    -- , regexTestHelper "(a)((b)(c))(d)" (CaptureGroup 1 (CaptureGroup 2 (Literal "a")))
    ]
  , testGroup "Parsing Choice only"
    [ regexTestHelper "a|b" (BinChoice (Literal "a") (Literal "b"))
    , regexTestHelper "abc|def" (BinChoice (Sequence [Literal "a",Literal "b", Literal "c"]) (Sequence [Literal "d",Literal "e", Literal "f"]))
    , regexTestHelper "a|b|c" (BinChoice (Literal "a") (BinChoice (Literal "b") (Literal "c")))
    , regexTestHelper "a|bc|d" (BinChoice (Literal "a") (BinChoice (Sequence [Literal "b", Literal "c"]) (Literal "d")))
    , regexTestHelper "a|b|c|d" (BinChoice (Literal "a") (BinChoice (Literal "b") (BinChoice (Literal "c") (Literal "d"))))
    ]
  , parseStringToTreeRepetition
  , testGroup "Parsing Capture Group and Choice"
    [ regexTestHelper "(a|b)" (CaptureGroup 1 (BinChoice (Literal "a") (Literal "b")))
    , regexTestHelper "(a)|b" (BinChoice (CaptureGroup 1 (Literal "a")) (Literal "b"))
    , regexTestHelper "a|(b)" (BinChoice (Literal "a") (CaptureGroup 1 (Literal "b")))
    , regexTestHelper "(a)|(b)" (BinChoice (CaptureGroup 1 (Literal "a")) (CaptureGroup 2 (Literal "b")))
    ]
  , testGroup "Parsing Capture Group and Repetition Operators"
    [ regexTestHelper "(abc)?" (Repetition False 0 (Just 1) (CaptureGroup 1 (Sequence [Literal "a", Literal "b", Literal "c"])))
    , regexTestHelper "(abc)??" (Repetition True 0 (Just 1) (CaptureGroup 1 (Sequence [Literal "a", Literal "b", Literal "c"])))
    , regexTestHelper "a(bc)?" (Sequence [Literal "a", Repetition False 0 (Just 1) (CaptureGroup 1 (Sequence [Literal "b", Literal "c"]))])
    , regexTestHelper "a(bc)??" (Sequence [Literal "a", Repetition True 0 (Just 1) (CaptureGroup 1 (Sequence [Literal "b", Literal "c"]))])
    , regexTestHelper "a(bc)?d" (Sequence [Literal "a", Repetition False 0 (Just 1) (CaptureGroup 1 (Sequence [Literal "b", Literal "c"])), Literal "d"])
    , regexTestHelper "a(bc)??d" (Sequence [Literal "a", Repetition True 0 (Just 1) (CaptureGroup 1 (Sequence [Literal "b", Literal "c"])), Literal "d"])
    ]
  , testGroup "Parsing Choice and Repetition Operators"
    []
  , testGroup "Any Valid Regex"
    []
  ]

parseStringToTreeRepetition :: TestTree
parseStringToTreeRepetition = testGroup "Parsing Repetition Operators"
  [ testGroup "Parsing Kleene Star"
    [ regexTestHelper "a*" (Repetition False 0 Nothing (Literal "a"))
    , regexTestHelper "a*?" (Repetition True 0 Nothing (Literal "a"))
    , regexTestHelper "ab*" (Sequence [Literal "a", Repetition False 0 Nothing (Literal "b")])
    , regexTestHelper "ab*?" (Sequence [Literal "a", Repetition True 0 Nothing (Literal "b")])
    , regexTestHelper "a*b" (Sequence [Repetition False 0 Nothing (Literal "a"), Literal "b"])
    , regexTestHelper "a*?b" (Sequence [Repetition True 0 Nothing (Literal "a"), Literal "b"])
    , regexTestHelper "ab*c" (Sequence [Literal "a", Repetition False 0 Nothing (Literal "b"), Literal "c"])
    , regexTestHelper "ab*?c" (Sequence [Literal "a", Repetition True 0 Nothing (Literal "b"), Literal "c"])
    ]
  , testGroup "Parsing Plus"
    [ regexTestHelper "a+" (Repetition False 1 Nothing (Literal "a"))
    , regexTestHelper "a+?" (Repetition True 1 Nothing (Literal "a"))
    , regexTestHelper "ab+" (Sequence [Literal "a", Repetition False 1 Nothing (Literal "b")])
    , regexTestHelper "ab+?" (Sequence [Literal "a", Repetition True 1 Nothing (Literal "b")])
    , regexTestHelper "a+b" (Sequence [Repetition False 1 Nothing (Literal "a"), Literal "b"])
    , regexTestHelper "a+?b" (Sequence [Repetition True 1 Nothing (Literal "a"), Literal "b"])
    , regexTestHelper "ab+c" (Sequence [Literal "a", Repetition False 1 Nothing (Literal "b"), Literal "c"])
    , regexTestHelper "ab+?c" (Sequence [Literal "a", Repetition True 1 Nothing (Literal "b"), Literal "c"])
    ]
  , testGroup "Parsing Optional"
    [regexTestHelper "a?" (Repetition False 0 (Just 1) (Literal "a"))
    , regexTestHelper "a??" (Repetition True 0 (Just 1) (Literal "a"))
    , regexTestHelper "ab?" (Sequence [Literal "a", Repetition False 0 (Just 1) (Literal "b")])
    , regexTestHelper "ab??" (Sequence [Literal "a", Repetition True 0 (Just 1) (Literal "b")])
    , regexTestHelper "a?b" (Sequence [Repetition False 0 (Just 1) (Literal "a"), Literal "b"])
    , regexTestHelper "a??b" (Sequence [Repetition True 0 (Just 1) (Literal "a"), Literal "b"])
    , regexTestHelper "ab?c" (Sequence [Literal "a", Repetition False 0 (Just 1) (Literal "b"), Literal "c"])
    , regexTestHelper "ab??c" (Sequence [Literal "a", Repetition True 0 (Just 1) (Literal "b"), Literal "c"])
    ]
  ]


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
      [ testCase "Parse Epsilon"  (strSolParse "``" @?= Right (ValExp (RegexVal False "")))
      , testCase "Parse Single Literal"  (strSolParse "`a`" @?= Right (ValExp (RegexVal False "a")))
      , testCase "Parse Sequence of Literal"  (strSolParse "`abc`" @?= Right (ValExp (RegexVal False "abc")))
      , testCase "Parse Simple Regex"  (strSolParse "`[abc]`" @?= Right (ValExp (RegexVal False "[abc]")))
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

