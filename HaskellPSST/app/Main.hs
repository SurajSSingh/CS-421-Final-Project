module Main where
import PSST.Runtime
import Data.HashMap.Strict as H ( fromList )

runtime = H.fromList []

main :: IO ()
main = repl main runtime
