-- Repl code adapted from https://blogg.bekk.no/creating-a-repl-in-haskell-efcdef1deec2
module PSST.Repl 
    (read_
    ,eval_
    ,print_
    ,repl
    ) where

import PSST.Core()
import System.IO
import Control.Monad

read_ :: IO String
read_ = putStr "PSST-REPL >>> "
     >> hFlush stdout
     >> getLine

eval_ :: String -> String
eval_ input = input

print_ :: String -> IO ()
print_ = putStrLn

repl :: IO () -> IO ()
repl main = do
    input <- read_
    
    unless (input == "exit" || input == "stop" || input == "end" || input == "quit")
         $ print_ (eval_ input) >> main