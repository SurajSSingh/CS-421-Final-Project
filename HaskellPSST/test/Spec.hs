-- import RegexTests ( regexTreeTest )
import ParserTests ( parserTests )
import EvalTests ( evaluatorTests )
import Test.Tasty (TestTree, testGroup, defaultMain)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parserTests] -- [regexTreeTest, parserTests, evaluatorTests]
