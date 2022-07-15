module PSST.Runtime (repl) where

import System.IO
import Data.HashMap.Strict as H

import PSST.Core
import PSST.Parser
import PSST.Evaluator

runtime = H.fromList []

replPrompt :: String
replPrompt = "StrSol-REPL >>> "

quitCommands :: [String]
quitCommands = [ "quit", "exit"]

readString :: IO String
readString = putStr replPrompt
        >> hFlush stdout
        >> getLine

repl :: IO () -> IO ()
repl main = do
    input <- readString

    if input `elem` quitCommands
        then do
            print "Closing Solver" 
            return ()
        else
            case strSolParse input of
                Right exp ->
                    case eval exp runtime of
                    Right val -> print val
                    Left eErr -> print eErr
                Left pErr -> print pErr
        >> main