module ParserTests where
import PSST.Core
import PSST.Parser
import Text.Parsec.Error (ParseError)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

parserTests :: TestTree
parserTests = testGroup "Parser Tests" [parseStringToRegex]--[parseStringToTree, parseExpTest, parseErrorTest]

nodeWrapper :: RegexSequence -> Either ParseError RegexNode
nodeWrapper nodes = Right $ CaptureGroupSequence 0 nodes

regexNodeTestCaseHelper :: String -> RegexSequence -> TestTree
regexNodeTestCaseHelper regexStr regexNode = testCase ("Parsing " ++ regexStr) (strSolParseRegex ("\"" ++ regexStr ++ "\"") @?= nodeWrapper regexNode)

parseStringToRegex :: TestTree
parseStringToRegex = testGroup "Parse Regex String to Regex Node" 
    [ regularLiteralNodeParseTest
    , specialNodeParseTest
    , sequenceNodeParseTest
    , captureGroupParseTest
    , captureGroupStubParseTest
    , complementNodeParseTest
    , choiceNodeParseTest
    , symbolRepetitionParseTest
    , curlyRepetitionNodeParseTest
    ]


regularLiteralNodeParseTest :: TestTree
regularLiteralNodeParseTest = testGroup "Regular Literal Nodes" 
    [ regexNodeTestCaseHelper "a" [sCharNode "a"]
    , regexNodeTestCaseHelper "B" [sCharNode "B"]
    , regexNodeTestCaseHelper "1" [sCharNode "1"]
    , regexNodeTestCaseHelper "%" [sCharNode "%"]
    , regexNodeTestCaseHelper "!" [sCharNode "!"]
    ]

specialNodeParseTest :: TestTree
specialNodeParseTest = testGroup "Regular Literal Nodes" 
    [ regexNodeTestCaseHelper "." [anyCharNode]
    , regexNodeTestCaseHelper "" [epsilonNode]
    , regexNodeTestCaseHelper "\\." [sCharNode "\\."]
    , regexNodeTestCaseHelper "\\$" [sCharNode "\\$"]
    , regexNodeTestCaseHelper "\\|" [sCharNode "\\|"]
    , regexNodeTestCaseHelper "\\~" [sCharNode "\\~"]
    , regexNodeTestCaseHelper "\\?" [sCharNode "\\?"]
    , regexNodeTestCaseHelper "\\*" [sCharNode "\\*"]
    , regexNodeTestCaseHelper "\\+" [sCharNode "\\+"]
    , regexNodeTestCaseHelper "\\(" [sCharNode "\\("]
    , regexNodeTestCaseHelper "\\)" [sCharNode "\\)"]
    , regexNodeTestCaseHelper "\\{" [sCharNode "\\{"]
    , regexNodeTestCaseHelper "\\}" [sCharNode "\\}"]
    , regexNodeTestCaseHelper "\\\\" [sCharNode "\\\\"]
    , regexNodeTestCaseHelper "\\t" [sCharNode "\\t"]
    , regexNodeTestCaseHelper "\\n" [sCharNode "\\n"]
    ]

sequenceNodeParseTest :: TestTree
sequenceNodeParseTest = testGroup "Sequence (Single Capture Group) Nodes" 
    [ regexNodeTestCaseHelper "ab" [sCharNode "a", sCharNode "b"]
    , regexNodeTestCaseHelper "Cd1" [sCharNode "C", sCharNode "d", sCharNode "1"]
    , regexNodeTestCaseHelper "123" [sCharNode "1", sCharNode "2", sCharNode "3"]
    , regexNodeTestCaseHelper "Hello!" [sCharNode "H", sCharNode "e", sCharNode "l", sCharNode "l", sCharNode "o", sCharNode "!"]
    , regexNodeTestCaseHelper "\\.\\.\\." [sCharNode "\\.", sCharNode "\\.", sCharNode "\\."]
    ]

captureGroupParseTest :: TestTree
captureGroupParseTest = testGroup "Multiple Capture Group Nodes" 
    [ regexNodeTestCaseHelper "(a)" [CaptureGroupSequence 1 [sCharNode "a"]]
    , regexNodeTestCaseHelper "(ab)" [CaptureGroupSequence 1 [sCharNode "a", sCharNode "b"]]
    , regexNodeTestCaseHelper "(a)b" [CaptureGroupSequence 1 [sCharNode "a"], sCharNode "b"]
    , regexNodeTestCaseHelper "a(b)" [sCharNode "a", CaptureGroupSequence 1 [sCharNode "b"]]
    ]

captureGroupStubParseTest :: TestTree
captureGroupStubParseTest = testGroup "Capture Group Stub Nodes" 
    [ regexNodeTestCaseHelper "$1" [captureGroupStub 1]
    , regexNodeTestCaseHelper "($2)$3" [CaptureGroupSequence 1 [captureGroupStub 2], captureGroupStub 3]
    ]

complementNodeParseTest :: TestTree
complementNodeParseTest = testGroup "Complement Nodes" 
    [ regexNodeTestCaseHelper "~a" [ComplementNode $ sCharNode "a"]
    , regexNodeTestCaseHelper "~ab" [ComplementNode $ sCharNode "a", sCharNode "b"]
    , regexNodeTestCaseHelper "~(ab)" [ComplementNode $ CaptureGroupSequence 1 [sCharNode "a", sCharNode "b"]]
    ]

choiceNodeParseTest :: TestTree
choiceNodeParseTest = testGroup "Choice Nodes" 
    [ regexNodeTestCaseHelper "a|b" [ChoiceNode (sCharNode "a") (sCharNode "b")]
    ,regexNodeTestCaseHelper "(a|b)" [CaptureGroupSequence 1 [ChoiceNode (sCharNode "a") (sCharNode "b")]]
    , regexNodeTestCaseHelper "a|oh" [ChoiceNode (sCharNode "a") (sCharNode "o"), sCharNode "h"]
    , regexNodeTestCaseHelper "ox|z" [sCharNode "o", ChoiceNode (sCharNode "x") (sCharNode "z")]
    , regexNodeTestCaseHelper "gre|ay" [sCharNode "g", sCharNode "r", ChoiceNode (sCharNode "e") (sCharNode "a"), sCharNode "y"]
    , regexNodeTestCaseHelper "(ax)|(by)" [ChoiceNode (CaptureGroupSequence 1 [sCharNode "a", sCharNode "x"]) (CaptureGroupSequence 2 [sCharNode "b", sCharNode "y"])]
    ]

symbolRepetitionParseTest :: TestTree
symbolRepetitionParseTest = testGroup "Symbol Repetition Nodes" 
    [ regexNodeTestCaseHelper "a*" [RepetitionNode False 0 Nothing $ sCharNode "a"]
    , regexNodeTestCaseHelper "a*?" [RepetitionNode True 0 Nothing $ sCharNode "a"]
    , regexNodeTestCaseHelper "a+" [RepetitionNode False 1 Nothing $ sCharNode "a"]
    , regexNodeTestCaseHelper "a+?" [RepetitionNode True 1 Nothing $ sCharNode "a"]
    , regexNodeTestCaseHelper "a?" [RepetitionNode False 0 (Just 1) $ sCharNode "a"]
    , regexNodeTestCaseHelper "a??" [RepetitionNode True 0 (Just 1) $ sCharNode "a"]
    , regexNodeTestCaseHelper "(ab)*" [RepetitionNode False 0 Nothing $ CaptureGroupSequence 1 [sCharNode "a", sCharNode "b"]]
    , regexNodeTestCaseHelper "(ab)*?" [RepetitionNode True 0 Nothing $ CaptureGroupSequence 1 [sCharNode "a", sCharNode "b"]]
    , regexNodeTestCaseHelper "(ab)+" [RepetitionNode False 1 Nothing $ CaptureGroupSequence 1 [sCharNode "a", sCharNode "b"]]
    , regexNodeTestCaseHelper "(ab)+?" [RepetitionNode True 1 Nothing $ CaptureGroupSequence 1 [sCharNode "a", sCharNode "b"]]
    , regexNodeTestCaseHelper "(ab)?" [RepetitionNode False 0 (Just 1) $ CaptureGroupSequence 1 [sCharNode "a", sCharNode "b"]]
    , regexNodeTestCaseHelper "(ab)??" [RepetitionNode True 0 (Just 1) $ CaptureGroupSequence 1 [sCharNode "a", sCharNode "b"]]
    ]

curlyRepetitionNodeParseTest :: TestTree
curlyRepetitionNodeParseTest = testGroup "Curly Repetition Nodes"
    [ regexNodeTestCaseHelper "a{1,2}" [RepetitionNode False 1 (Just 2) $ sCharNode "a"]
    , regexNodeTestCaseHelper "a{1,2}?" [RepetitionNode True 1 (Just 2) $ sCharNode "a"]
    , regexNodeTestCaseHelper "a{2,}" [RepetitionNode False 2 Nothing $ sCharNode "a"]
    , regexNodeTestCaseHelper "a{2,}?" [RepetitionNode True 2 Nothing $ sCharNode "a"]
    , regexNodeTestCaseHelper "a{2}" [RepetitionNode False 2 (Just 2) $ sCharNode "a"]
    , regexNodeTestCaseHelper "a{2}?" [RepetitionNode True 2 (Just 2) $ sCharNode "a"]
    , regexNodeTestCaseHelper "ab{1,2}" [sCharNode "a", RepetitionNode False 1 (Just 2) $ sCharNode "b"]
    , regexNodeTestCaseHelper "ab{1,2}?" [sCharNode "a", RepetitionNode True 1 (Just 2) $ sCharNode "b"]
    , regexNodeTestCaseHelper "ab{2,}" [sCharNode "a", RepetitionNode False 2 Nothing $ sCharNode "b"]
    , regexNodeTestCaseHelper "ab{2,}?" [sCharNode "a", RepetitionNode True 2 Nothing $ sCharNode "b"]
    , regexNodeTestCaseHelper "ab{2}" [sCharNode "a", RepetitionNode False 2 (Just 2) $ sCharNode "b"]
    , regexNodeTestCaseHelper "ab{2}?" [sCharNode "a", RepetitionNode True 2 (Just 2) $ sCharNode "b"]
    , regexNodeTestCaseHelper "(ab){1,2}" [RepetitionNode False 1 (Just 2) $ CaptureGroupSequence 1 [sCharNode "a", sCharNode "b"]]
    , regexNodeTestCaseHelper "(ab){1,2}?" [RepetitionNode True 1 (Just 2) $ CaptureGroupSequence 1 [sCharNode "a", sCharNode "b"]]
    , regexNodeTestCaseHelper "(ab){2,}" [RepetitionNode False 2 Nothing $ CaptureGroupSequence 1 [sCharNode "a", sCharNode "b"]]
    , regexNodeTestCaseHelper "(ab){2,}?" [RepetitionNode True 2 Nothing $ CaptureGroupSequence 1 [sCharNode "a", sCharNode "b"]]
    , regexNodeTestCaseHelper "(ab){2}" [RepetitionNode False 2 (Just 2) $ CaptureGroupSequence 1 [sCharNode "a", sCharNode "b"]]
    , regexNodeTestCaseHelper "(ab){2}?" [RepetitionNode True 2 (Just 2) $ CaptureGroupSequence 1 [sCharNode "a", sCharNode "b"]]
    ]
