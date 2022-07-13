module PSST.Core where

import qualified Data.HashMap.Strict as H


type Env = H.HashMap String Val

data Val = RegexVal String
         | CharListVal String
         | StringVal String
         | PredicateVal Bool

instance Show Val where
    show (RegexVal str) = "regex: " ++ show str
    show (CharListVal str) = "alphabet: " ++ show str
    show (StringVal str) = "string: " ++ show str
    show (PredicateVal str) = show str

errorPrefix :: [Char]
errorPrefix = "[ERROR] "

data Diagnostic = UnimplementedError String
                | InvalidExpression Exp
                | InvalidValue Val
                | UnknownVariable String

instance Show Diagnostic where
    show (UnimplementedError feature) = errorPrefix ++ "Feature Unimplemented: " ++ show feature
    show (InvalidExpression expr) = errorPrefix ++ "Invalid Expression: " ++ show expr
    show (InvalidValue val) = errorPrefix ++ "Invalid Value: " ++ show val
    show (UnknownVariable var) = errorPrefix ++ "Variable not defined: " ++ show var
    -- show () = errorPrefix ++ ""


data Exp = ValExp Val
         | AssignmentExp String Exp
         | VarExp String
         | SequenceExp [Exp]
         | IfExp Val Exp Exp
         | BinOpExp String Exp Exp
         | AtomFuncExp String [String] Exp Env
         | UserFuncExp String [String] Exp Env

instance Show Exp where
    show (ValExp val) = show val
    show (AssignmentExp var exp) = "Assigning " ++ var ++ " to " ++ show exp
    show (VarExp var) = "Getting " ++ var
    show (SequenceExp exps) = "Sequence of Expression: " ++ unwords ( map (\v -> show v ++ "\n") exps)
    show (IfExp bool true false) = "If Expression: " ++ show bool ++ ", \ntrue: " ++ show true ++ ", \nfalse: " ++ show false
    show (BinOpExp op e1 e2) = "Binary Operation: " ++ show e1 ++ " " ++ show op ++ " " ++ show e2
    show (AtomFuncExp name args exp env) = "Builtin Function: " ++ name ++ " with " ++ show args
    show (UserFuncExp name args exp env) = "User Defined Function: " ++ name ++ " with " ++ show args