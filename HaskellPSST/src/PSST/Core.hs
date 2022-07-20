module PSST.Core where
import Data.HashMap.Strict as H
import Control.Monad.State
import Control.Monad.Except
import Data.List
import Data.Maybe

---
requireEscapeRegexSymbol :: [String]
requireEscapeRegexSymbol = ["(", ")", "|", "?", "*", "+", "$", "."]

escapedRegexSymbol :: [String]
escapedRegexSymbol = ["\\(", "\\)", "\\|", "\\?", "\\*", "\\+", "\\$", "\\."]

--- ### Environment
type Env = H.HashMap String [Exp]
type EvalState a = StateT Env (Except Diagnostic) a

--- ### Regex Tree
--- Nodes: Empty, epsilon, (sigma - any char), exact string, numbered capture group, repetition (lazy or greedy) from M to N?
--- Adapted from Definition 3.1 of 
data RegexTree = EmptySet
               | Epsilon
               | AnyCharLiteral
               | Literal String
               | CaptureGroup Int RegexTree
               | CaptureGroupStub (Maybe Int)
               | Sequence [RegexTree]
               | BinChoice RegexTree RegexTree
               | Repetition Bool Int (Maybe Int) RegexTree
               deriving ( Eq
                        -- , Show
                        )

instance Show RegexTree where
    show EmptySet = "{}"
    show Epsilon = ""
    show AnyCharLiteral = "."
    show (Literal lit) = if lit `elem` requireEscapeRegexSymbol then "\\" ++ lit else lit
    show (Sequence seq) =  concatMap show seq
    show (BinChoice n1 n2) = show n1 ++ "|" ++ show n2
    show (CaptureGroup num node) = "<$"++ show num ++ ":("++ show node ++ ")>"
    show (CaptureGroupStub num) = case num of
        Nothing ->"<$INVALID STUB$>" 
        Just n -> "<$"++ show n ++ ">"
    show (Repetition isLazy start end node) =
        let
        lazySym = if isLazy then "?" else ""
        repeatSym = case (start, end) of
            (0,Just 1) -> "?"
            (0,Nothing) -> "*"
            (1,Nothing) -> "+"
            (s ,Nothing) -> "{" ++ show s ++ "}"
            (s ,Just e) -> "{" ++ show s ++ "," ++ show e ++ "}"
        in
            show node ++ repeatSym ++ lazySym

--- ### Values
data Val = IntVal Int
         | ResultVal String
         | RegexVal Bool RegexTree
         | Void
         deriving (Eq)

instance Show Val where
    show (IntVal i) = show i
    show (ResultVal r) = r
    show (RegexVal b r) = (if b then "set-complement of " else "") ++ "<$0:(" ++ show r ++ ")>"
    show Void = show ""

--- ### Expressions
data Exp = ValExp Val
         | VarExp String
         | AssignmentExp String Exp
         | OperatorExp String Exp (Maybe Exp) (Maybe Exp)
         | StateOpExp String (Maybe String)
         deriving (Eq)


instance Show Exp where
    show (ValExp v) = show v
    show (VarExp v) = "var " ++ show v
    show (AssignmentExp var exp) = "Set var " ++ show var ++ " to (" ++ show exp ++ ")"
    show (OperatorExp op e1 e2 e3) = "Op " ++ show op ++ " with (" ++ opArgs
        where
            opArgs = show e1 ++ (case e2 of
               Nothing -> ")"
               Just v2 -> " and " ++ show v2 ++ (case e3 of
                  Nothing -> ")"
                  Just v3 -> " and " ++ show v3 ++ ")"))
    show (StateOpExp op var) = case var of
      Nothing -> "Running " ++ op ++ " on current environment"
      Just s -> "Running " ++ op ++ " on var " ++ show s

data Diagnostic = UnimplementedError String
                | InvalidOperation String
                | VariableNotFound String
                deriving (Eq)

instance Show Diagnostic where
    show (UnimplementedError x) = x ++ " Unimplemented"
    show (InvalidOperation x) = "Operation " ++ show x ++ " does not exist"
    show (VariableNotFound x) = "Variable " ++ show x ++ " is not defined"

unimplemented :: String -> EvalState a
unimplemented = throwError . UnimplementedError
