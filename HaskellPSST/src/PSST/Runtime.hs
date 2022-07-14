module PSST.Runtime (repl) where

import System.IO
import Data.HashMap.Strict as H

import PSST.Core
import PSST.Parser
import PSST.Evaluator

runtime = H.fromList []

readString :: IO String
readString = putStr "PSST-REPL >>> "
        >> hFlush stdout
        >> getLine

repl :: IO () -> IO ()
repl main = do
    input <- readString

    if input == "exit" || input == "quit"
        then return ()
        else
            case eval (parse input) runtime of
            Right val -> print val
            Left err -> print err
        >> main