module ParserTests where
import PSST.Core
import PSST.Parser
import Text.Parsec.Error (ParseError)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

parserTests :: TestTree
parserTests = testGroup "Parser Tests" [parseStringToRegex, parseStringToExpression]

nodeWrapper :: RegexSequence -> RegexNode
nodeWrapper = CaptureGroupSequence 0

correctNodeWrapper :: RegexSequence -> Either ParseError RegexNode
correctNodeWrapper nodes = Right $ nodeWrapper nodes

regexExpNodeWrapper :: RegexSequence -> Exp
regexExpNodeWrapper nodes = RegexExp $ nodeWrapper nodes

regexNodeTestCaseHelper :: String -> RegexSequence -> TestTree
regexNodeTestCaseHelper regexStr regexNode = testCase ("Parsing " ++ regexStr) (strSolParseRegex ("\"" ++ regexStr ++ "\"") @?= correctNodeWrapper regexNode)

parseStrToExpHelper :: String -> Exp -> TestTree
parseStrToExpHelper str exp = testCase ("Parsing " ++ str) (strSolParse str @?= Right exp)

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

parseStringToExpression :: TestTree
parseStringToExpression = testGroup "Parse String to Expression"
    [ parseStringToNumberExp
    , parseStringToVariableExp
    , parseStringToRegexExp
    , parseStringToAssignmentExp
    , parseStringToRegexOpExp
    , parseStringToStateOpExp
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
    , regexNodeTestCaseHelper "(abc)" [CaptureGroupSequence 1 [sCharNode "a", sCharNode "b", sCharNode "c"]]
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

parseStringToNumberExp :: TestTree
parseStringToNumberExp = testGroup "Parse String to Number"
    [ parseStrToExpHelper "1" $ IntExp 1
    , parseStrToExpHelper "10   " $ IntExp 10
    , parseStrToExpHelper "   21" $ IntExp 21
    , parseStrToExpHelper "   210   " $ IntExp 210
    ]

parseStringToVariableExp :: TestTree
parseStringToVariableExp = testGroup "Parse String to Variable"
    [ parseStrToExpHelper "x" $ VarExp "x"
    , parseStrToExpHelper "y1" $ VarExp "y1"
    , parseStrToExpHelper "z_" $ VarExp "z_"
    , parseStrToExpHelper "x_y" $ VarExp "x_y"
    , parseStrToExpHelper "x_1z" $ VarExp "x_1z"
    , parseStrToExpHelper "y_z1" $ VarExp "y_z1"
    ]

parseStringToRegexExp :: TestTree
parseStringToRegexExp = testGroup "Parse String to Regex"
    [ parseStrToExpHelper "\"\"" $ regexExpNodeWrapper [epsilonNode]
    , parseStrToExpHelper "\".\"" $ regexExpNodeWrapper [anyCharNode]
    , parseStrToExpHelper "\"a\"" $ regexExpNodeWrapper [sCharNode "a"]
    ]

parseStringToRegexOpExp :: TestTree
parseStringToRegexOpExp = testGroup "Parse String to Regex Operation"
    [ parseStrToExpHelper "single \".\"" $ OperatorExp "singleton" (regexExpNodeWrapper [anyCharNode]) Nothing Nothing
    , parseStrToExpHelper "single \"a\"" $ OperatorExp "singleton" (regexExpNodeWrapper [sCharNode "a"]) Nothing Nothing
    , parseStrToExpHelper "single \"\"" $ OperatorExp "singleton" (regexExpNodeWrapper [epsilonNode]) Nothing Nothing
    , parseStrToExpHelper ":S \".\"" $ OperatorExp "singleton" (regexExpNodeWrapper [anyCharNode]) Nothing Nothing
    , parseStrToExpHelper ":S \"a\"" $ OperatorExp "singleton" (regexExpNodeWrapper [sCharNode "a"]) Nothing Nothing
    , parseStrToExpHelper ":S \"\"" $ OperatorExp "singleton" (regexExpNodeWrapper [epsilonNode]) Nothing Nothing
    , parseStrToExpHelper "subset \".\" \"\"" $ OperatorExp "subset" (regexExpNodeWrapper [anyCharNode]) (Just (regexExpNodeWrapper [epsilonNode])) Nothing
    , parseStrToExpHelper ":s \".\" \"\"" $ OperatorExp "subset" (regexExpNodeWrapper [anyCharNode]) (Just (regexExpNodeWrapper [epsilonNode])) Nothing
    , parseStrToExpHelper "extract 1 \".\" \"a\"" $ OperatorExp "extract" (IntExp 1) (Just $ regexExpNodeWrapper [anyCharNode]) (Just (regexExpNodeWrapper [sCharNode "a"]))
    , parseStrToExpHelper ":e 1 \".\" \"a\"" $ OperatorExp "extract" (IntExp 1) (Just $ regexExpNodeWrapper [anyCharNode]) (Just (regexExpNodeWrapper [sCharNode "a"]))
    , parseStrToExpHelper "replace \".\" \"\" \"a\"" $ OperatorExp "replace" (regexExpNodeWrapper [anyCharNode]) (Just (regexExpNodeWrapper [epsilonNode])) (Just (regexExpNodeWrapper [sCharNode "a"]))
    , parseStrToExpHelper ":r \".\" \"\" \"a\"" $ OperatorExp "replace" (regexExpNodeWrapper [anyCharNode]) (Just (regexExpNodeWrapper [epsilonNode])) (Just (regexExpNodeWrapper [sCharNode "a"]))
    , parseStrToExpHelper "replaceAll \".\" \"\" \"a\"" $ OperatorExp "replaceAll" (regexExpNodeWrapper [anyCharNode]) (Just (regexExpNodeWrapper [epsilonNode])) (Just (regexExpNodeWrapper [sCharNode "a"]))
    , parseStrToExpHelper ":R \".\" \"\" \"a\"" $ OperatorExp "replaceAll" (regexExpNodeWrapper [anyCharNode]) (Just (regexExpNodeWrapper [epsilonNode])) (Just (regexExpNodeWrapper [sCharNode "a"]))
    , parseStrToExpHelper "union \".\" \"\"" $ OperatorExp "union" (regexExpNodeWrapper [anyCharNode]) (Just (regexExpNodeWrapper [epsilonNode])) Nothing
    , parseStrToExpHelper ":u \".\" \"\"" $ OperatorExp "union" (regexExpNodeWrapper [anyCharNode]) (Just (regexExpNodeWrapper [epsilonNode])) Nothing
    , parseStrToExpHelper ":u \".\" :u \"\" \"a\"" $ OperatorExp "union" (regexExpNodeWrapper [anyCharNode]) (Just (OperatorExp "union" (regexExpNodeWrapper [epsilonNode]) (Just (regexExpNodeWrapper [sCharNode "a"])) Nothing)) Nothing
    , parseStrToExpHelper "unify \".\" \"\"" $ OperatorExp "unify" (regexExpNodeWrapper [anyCharNode]) (Just (regexExpNodeWrapper [epsilonNode])) Nothing
    , parseStrToExpHelper ":n \".\" \"\"" $ OperatorExp "unify" (regexExpNodeWrapper [anyCharNode]) (Just (regexExpNodeWrapper [epsilonNode])) Nothing
    , parseStrToExpHelper ":n \".\" :n \"\" \"a\"" $ OperatorExp "unify" (regexExpNodeWrapper [anyCharNode]) (Just (OperatorExp "unify" (regexExpNodeWrapper [epsilonNode]) (Just (regexExpNodeWrapper [sCharNode "a"])) Nothing)) Nothing
    , parseStrToExpHelper "\".\" + \"\"" $ OperatorExp "concat" (regexExpNodeWrapper [anyCharNode]) (Just (regexExpNodeWrapper [epsilonNode])) Nothing
    , parseStrToExpHelper "\".\" + \"\" + \"a\"" $ OperatorExp "concat" (regexExpNodeWrapper [anyCharNode]) (Just (OperatorExp "concat" (regexExpNodeWrapper [epsilonNode]) (Just (regexExpNodeWrapper [sCharNode "a"])) Nothing)) Nothing
    ]

parseStringToAssignmentExp :: TestTree
parseStringToAssignmentExp = testGroup "Parse String to Assignment Operation" 
    [parseStringToAssignmentSimple, parseStringToAssignmentWithOperator]

parseStringToAssignmentSimple :: TestTree
parseStringToAssignmentSimple = testGroup "Parse String with Simple Assignment" 
    [ parseStrToExpHelper "x = \"\"" $ AssignmentExp "x" (regexExpNodeWrapper [epsilonNode])
    , parseStrToExpHelper "y_1 = \".\"" $ AssignmentExp "y_1" (regexExpNodeWrapper [anyCharNode])
    , parseStrToExpHelper "z = a" $ AssignmentExp "z" (VarExp "a")
    ]

parseStringToAssignmentWithOperator :: TestTree
parseStringToAssignmentWithOperator = testGroup "Parse String with Complex Assignment" 
    [ parseStrToExpHelper "x = \".\" + \"\"" $ AssignmentExp "x" (OperatorExp "concat" (regexExpNodeWrapper [anyCharNode]) (Just (regexExpNodeWrapper [epsilonNode])) Nothing)
    , parseStrToExpHelper "x = \".\" + \"\" + \"a\"" $ AssignmentExp "x" (OperatorExp "concat" (regexExpNodeWrapper [anyCharNode]) (Just (OperatorExp "concat" (regexExpNodeWrapper [epsilonNode]) (Just (regexExpNodeWrapper [sCharNode "a"])) Nothing)) Nothing)
    , parseStrToExpHelper "x = extract 1 \".\" \"a\"" $ AssignmentExp "x" (OperatorExp "extract" (IntExp 1) (Just $ regexExpNodeWrapper [anyCharNode]) (Just (regexExpNodeWrapper [sCharNode "a"])))
    , parseStrToExpHelper "x = :e 1 \".\" \"a\"" $ AssignmentExp "x" (OperatorExp "extract" (IntExp 1) (Just $ regexExpNodeWrapper [anyCharNode]) (Just (regexExpNodeWrapper [sCharNode "a"])))
    , parseStrToExpHelper "x = replace \".\" \"\" \"a\"" $ AssignmentExp "x" (OperatorExp "replace" (regexExpNodeWrapper [anyCharNode]) (Just (regexExpNodeWrapper [epsilonNode])) (Just (regexExpNodeWrapper [sCharNode "a"])))
    , parseStrToExpHelper "x = :r \".\" \"\" \"a\"" $ AssignmentExp "x" (OperatorExp "replace" (regexExpNodeWrapper [anyCharNode]) (Just (regexExpNodeWrapper [epsilonNode])) (Just (regexExpNodeWrapper [sCharNode "a"])))
    , parseStrToExpHelper "x = replaceAll \".\" \"\" \"a\"" $ AssignmentExp "x" (OperatorExp "replaceAll" (regexExpNodeWrapper [anyCharNode]) (Just (regexExpNodeWrapper [epsilonNode])) (Just (regexExpNodeWrapper [sCharNode "a"])))
    , parseStrToExpHelper "x = :R \".\" \"\" \"a\"" $ AssignmentExp "x" (OperatorExp "replaceAll" (regexExpNodeWrapper [anyCharNode]) (Just (regexExpNodeWrapper [epsilonNode])) (Just (regexExpNodeWrapper [sCharNode "a"])))
    , parseStrToExpHelper "x = union \".\" \"\"" $ AssignmentExp "x" (OperatorExp "union" (regexExpNodeWrapper [anyCharNode]) (Just (regexExpNodeWrapper [epsilonNode])) Nothing)
    , parseStrToExpHelper "x = :u \".\" \"\"" $ AssignmentExp "x" (OperatorExp "union" (regexExpNodeWrapper [anyCharNode]) (Just (regexExpNodeWrapper [epsilonNode])) Nothing)
    , parseStrToExpHelper "x = :u \".\" :u \"\" \"a\"" $ AssignmentExp "x" (OperatorExp "union" (regexExpNodeWrapper [anyCharNode]) (Just (OperatorExp "union" (regexExpNodeWrapper [epsilonNode]) (Just (regexExpNodeWrapper [sCharNode "a"])) Nothing)) Nothing)
    , parseStrToExpHelper "x = unify \".\" \"\"" $ AssignmentExp "x" (OperatorExp "unify" (regexExpNodeWrapper [anyCharNode]) (Just (regexExpNodeWrapper [epsilonNode])) Nothing)
    , parseStrToExpHelper "x = :n \".\" \"\"" $ AssignmentExp "x" (OperatorExp "unify" (regexExpNodeWrapper [anyCharNode]) (Just (regexExpNodeWrapper [epsilonNode])) Nothing)
    , parseStrToExpHelper "x = :n \".\" :n \"\" \"a\"" $ AssignmentExp "x" (OperatorExp "unify" (regexExpNodeWrapper [anyCharNode]) (Just (OperatorExp "unify" (regexExpNodeWrapper [epsilonNode]) (Just (regexExpNodeWrapper [sCharNode "a"])) Nothing)) Nothing)
    ]

parseStringToStateOpExp :: TestTree
parseStringToStateOpExp = testGroup "Parse String to State Operation"
    [ parseStrToExpHelper "check" $ StateOpExp "check" Nothing
    , parseStrToExpHelper "check x" $ StateOpExp "check" (Just $ VarExp"x")
    , parseStrToExpHelper "solve" $ StateOpExp "solve" Nothing
    , parseStrToExpHelper "solve x" $ StateOpExp "solve" (Just $ VarExp"x")
    , parseStrToExpHelper "clear" $ StateOpExp "clear" Nothing
    , parseStrToExpHelper "clear x" $ StateOpExp "clear" (Just $ VarExp"x")
    , parseStrToExpHelper "state" $ StateOpExp "state" Nothing
    , parseStrToExpHelper "state x" $ StateOpExp "state" (Just $ VarExp"x")
    ]