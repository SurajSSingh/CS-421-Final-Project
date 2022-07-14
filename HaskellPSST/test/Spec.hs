import PSST.Transducer
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain simpleMathTests


simpleMathFunction :: Int -> Int -> Int -> Int
simpleMathFunction a b c = a * b - c

simpleMathTests :: TestTree
simpleMathTests = testGroup "Simple Math Tests"
  [ testCase "Small Numbers" $
      simpleMathFunction 3 4 5 @?= 7,
    testCase "Medium Numbers" $
        simpleMathFunction 10 20 30 @?= 170
  ]

