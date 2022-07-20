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

data AtLeastTwoList a = AtLeastTwoList {first2PValue :: a, second2PValue :: a, rest2PValue :: [a]}
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

--- ### Helper Functions for Regex Tree building
---     Allows these assumptions:
---         1. Sequence will contain at least two items
---         2. Repetition will apply only to the last item before it
---         3. Epsilon will be removed, unless it's literally the only thing
---         4. Binary choice will have two distinct items, with neither being subset of each other
regexTreeSeqHelper :: [RegexTree] -> RegexTree
regexTreeSeqHelper [] = Epsilon -- Empty sequence is epsilon
regexTreeSeqHelper [tree] = tree -- Return single item as itself (no sequence needed)
regexTreeSeqHelper trees@(t:ts) = case t of
    Epsilon -> regexTreeSeqHelper ts -- Remove epsilon, continue
    _ -> case regexTreeSeqHelper ts of
      EmptySet -> t
      Epsilon -> t
      Sequence rts -> Sequence (t:rts)
      n -> Sequence [t, n]

regexTreeRepHelper ::  Bool -> Int -> Maybe Int -> [RegexTree] -> [RegexTree]
regexTreeRepHelper lazy start end [] = [Epsilon] -- Repeat epsilon as many times as you want, it's still just epsilon
regexTreeRepHelper lazy start end [tree] = [Repetition lazy start end tree] -- Last or only item, put repetition on it
regexTreeRepHelper lazy start end (t:ts) = case t of
    Epsilon -> regexTreeRepHelper lazy start end ts -- Remove epsilon, continue
    _ -> t : regexTreeRepHelper lazy start end ts

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
                | InvalidOperationError String
                | InvalidArgumentsError String [String]
                | NumOfArgumentsError String Int Int [String]
                | VariableNotFoundError String
                deriving (Eq)

instance Show Diagnostic where
    show (UnimplementedError x) = x ++ " Unimplemented"
    show (InvalidOperationError op) = "Operation " ++ show op ++ " does not exist"
    show (InvalidArgumentsError op args) = "Operation " ++ show op ++ " passed in invalid arguments: " ++ show args
    show (NumOfArgumentsError op expected given args) = "Operation " ++ show op ++ " passed in " ++ (if expected < given then "too many arguments, " else "too few arguments, ") ++ "given: " ++ show args
    show (VariableNotFoundError x) = "Variable " ++ show x ++ " is not defined"

unimplemented :: String -> EvalState a
unimplemented = throwError . UnimplementedError
