module PSST.Runtime (repl) where

import System.IO
import Data.HashMap.Strict as H

import PSST.Core
import PSST.Parser
import PSST.Evaluator
import Control.Monad.Except
import Control.Monad.State

replPrompt :: String
replPrompt = "StrSol-REPL >>> "

quitCommands :: [String]
quitCommands = [ "quit", "exit"]

readString :: IO String
readString = putStr replPrompt
        >> hFlush stdout
        >> getLine

repl :: IO () -> Env -> IO ()
repl main env = do
    input <- readString
    case input of 
        i | i `elem` quitCommands -> do 
            print "Closing Solver"
            return ()
        help | help == "help" -> do
            print "HELP:"
            repl main env
        _ -> case strSolParse input of
                Right exp ->
                    case runExcept $ runStateT (eval exp) env of
                    Right (val, newEnv) -> do
                        print val
                        repl main newEnv
                    Left eErr -> do
                        print eErr
                        repl main env
                Left pErr -> do
                    print pErr
                    repl main env