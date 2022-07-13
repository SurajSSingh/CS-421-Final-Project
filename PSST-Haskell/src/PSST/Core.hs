{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module PSST.Core where

import qualified Data.HashMap.Strict as H


type Env = H.HashMap String Val

data Val = RegexVal String
         | CharListVal String
         | StringVal String
         | BoolVal Bool

instance Show Val where
    show (RegexVal str) = "regex: " ++ show str
    show (CharListVal str) = "alphabet: " ++ show str
    show (StringVal str) = "string: " ++ show str
    show (BoolVal str) = show str

data Diagnostic = UnimplementedError
                | InvalidExpression Exp

instance Show Diagnostic where
    show UnimplementedError = "Unimplemented"
    show (InvalidExpression expr) = "Invalid Expression: " ++ show expr

data Exp = ValExp Val
         | AssignmentExp String Exp
         | VarExp String
         | SequenceExp [Exp]
         | IfExp Val Exp Exp
         | AtomFuncExp String [String] Exp Env
         | UserFuncExp String [String] Exp Env

instance Show Exp where
    show (ValExp val) = show val
    show (AssignmentExp var exp) = "Assigning " ++ var ++ " to " ++ show exp
    show (VarExp var) = "Getting " ++ var
    show (SequenceExp exps) = "Sequence of Expression: " ++ unwords ( map (\v -> show v ++ "\n") exps)
    show (IfExp bool true false) = "If Expression: " ++ show bool ++ ", \ntrue: " ++ show true ++ ", \nfalse: " ++ show false
    show (AtomFuncExp name args exp env) = "Builtin Function: " ++ name ++ " with " ++ show args
    show (UserFuncExp name args exp env) = "User Defined Function: " ++ name ++ " with " ++ show args