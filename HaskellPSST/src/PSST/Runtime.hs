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
            , "  * state <variable?>: Returns the current values assigned to variable(s). If provided with a variable,"
            , "                       it will only print out values assigned to that variable."
            , ""
            , "Regex Expressions:"
            , "  * Regex String is enclosed within double quotes: \"<string goes here>\""
            , "  * Syntax:"
            , "      * Literal:       Any non-escaped character, forms a group by itself if not within another capture group"
            , "      * Capture Group: Treat a sequence of literal characters as a single group, "
            , "                       wrapped between parenthesis '()', example: (ab)"
            , "      * Choice:        Use '|' symbol between two group, examples: a|b, (ab)|(cd)"
            , "      * Complement:    Use '~' symbol before group to signify not that group, example: ~a, ~(ab)"
            , "      * Repeat:        Allows repeating a group , written after the group as '{start, end?}'"
            , "                       where start is a natural number representing the minimum repetition and"
            , "                       end is an optional natural number representing maximum repetition (if not given,"
            , "                       then it allows infinite repetition); examples: a{1,10}, (ab){4,5}"
            , "      * Repeat Symbols:"
            , "          * '*' = {0, }, examples: a*, (cd)*"
            , "          * '+' = {1, }, examples: b+, (dc)+"
            , "          * '?' = {0,1}, examples: c?, (de)?"
            , "  * Special Characters (need to be escaped):"
            , "      * '.': Meta-character for any literal character"
            , "      * '$': Allows group number extraction ($0 means entire string)"
            , "      * Any regularly escaped characters like \\n and \\t"
            , "      * Any character used in the above regex syntax: '(', ')', '{', '}', '|', '~', '*', '+', '?'"
            , ""
            , "Operators:"
            , "  * Assignment:     Assign an expression to a variable. Takes the form: <var> = <exp>."
            , "                    <exp> can either be other variables, regex strings, "
            , "                    or the result of concatenation, union, or unify."
            , "  * Concatenation:  Concatenates two expressions together. Takes the form <exp1> + <exp2>."
            , "                    Allowed to chain multiple concatenations, example: x + y + z"
            , "  * Unify (n:):     Unifies two regex string by taking their set intersection. Either of the form:"
            , "                    unify <str1> <str2> or :n <str1> <str2>."
            , "                    Allowed to chain multiple unifies together, example: :n \"a*\" :n \"a+\" \"a?\""
            , "  * Union (u:):     Take the set union of two regex strings. Either of the form:"
            , "                    union <str1> <str2> or :u <str1> <str2>."
            , "                    Allowed to chain multiple unions together, example: :u \"a\" :u \"b\" \"c\""
            , "  * Singleton (:S): Checks if a given regex string is a singleton set for an accepting language."
            , "                    Either of the form: singleton <str> or :S <str>"
            , "  * Subset (:s):    Checks if a given regex string is a subset of another regex string."
            , "                    Either of the form: subset <str1> <str2> or :s <str1> <str2>"
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