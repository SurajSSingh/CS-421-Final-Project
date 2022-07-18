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

helpText :: [String]
helpText = ["Welcome to String Solver"
            , "Commands:"
            , "  * help:              Display this help text"
            , "  * check <variable?>: Check the current state of the solver to determine if it is satisfiable."
            , "                       If provided with an optional variable, it will do an internal check of that variable "
            , "                       (i.e. is the variable by itself satisfiable, assuming all other variables are?)"
            , "  * clear <variable?>: Clears the current state of the solver. If provided with an optional variable, "
            , "                       it will only clear that variable."
            , ""
            ]

repl :: IO () -> Env -> IO ()
repl main env = do
    input <- readString
    case input of 
        i | i `elem` quitCommands -> do 
            print "Closing Solver"
            return ()
        help | help == "help" -> do
            mapM_ putStrLn helpText
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