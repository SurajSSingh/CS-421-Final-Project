module PSST.Core where
import Data.HashMap.Strict as H
import Control.Monad.State
import Control.Monad.Except
import Data.List

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
               | Sequence [RegexTree]
               | BinChoice RegexTree RegexTree
            --    | Complement RegexTree
            --    | SetChoice [RegexTree]
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

--- ### Regex Tree pattern operations
--- #### Regex Unify: Return a regex tree that unifies two trees to their common pattern
--- ####              , will be empty set if cannot be unified
regexTreeUnify :: RegexTree -> RegexTree -> RegexTree
--- Empty set: Choose which ever is not empty
regexTreeUnify t1 EmptySet = t1
regexTreeUnify EmptySet t2 = t2
--- Capture Group: Referentially transparent to unification
---                ex: ab, a(b), (a)b, (ab) can all be unified into ab
regexTreeUnify t1 (CaptureGroup _ t2) = regexTreeUnify t1 t2
regexTreeUnify (CaptureGroup _ t1) t2 = regexTreeUnify t1 t2
--- Epsilons: Only epsilon can merge with other epsilons
regexTreeUnify Epsilon Epsilon = Epsilon
--- Literals and Any Literal:
--- Literals must match exactly, or match the any character literal
regexTreeUnify t1@(Literal l1) t2@(Literal l2)
    | l1 == l2 = t1
regexTreeUnify AnyCharLiteral t2@(Literal l) = t2
regexTreeUnify t1@(Literal l) AnyCharLiteral = t1

--- Choice
regexTreeUnify (BinChoice t1a t1b) (BinChoice t2a t2b) = case (regexTreeUnify t1a t2a, regexTreeUnify t1b t2b) of
    (EmptySet, t) -> t
    (t, EmptySet) -> t
    (nt1, nt2) -> BinChoice nt1 nt2
regexTreeUnify t1 (BinChoice t2a t2b) = regexTreeUnify (regexTreeUnify t1 t2a) (regexTreeUnify t1 t2b)
regexTreeUnify (BinChoice t1a t1b) t2 = regexTreeUnify (regexTreeUnify t1a t2) (regexTreeUnify t1b t2)
    
--- Repetition
    
--- Sequences

--- Otherwise, if it can't be unified, return empty set
regexTreeUnify _ _ = EmptySet

--- #### Is a given regex tree a subset of another regex tree
isRegexSubLang :: RegexTree -> RegexTree -> Bool
isRegexSubLang (Literal _) AnyCharLiteral = True
isRegexSubLang t (BinChoice t1 t2) = isRegexSubLang t t1 || isRegexSubLang t t2
isRegexSubLang EmptySet t = True
isRegexSubLang t1 t2 | t1 == t2 = True
isRegexSubLang _ _ = False

--- #### Does a given regex tree contain only a single valid string 
isRegexSingleton :: RegexTree -> Bool
isRegexSingleton (Literal _) = True
isRegexSingleton (Sequence [t]) = isRegexSingleton t
isRegexSingleton (CaptureGroup _ t) = isRegexSingleton t
isRegexSingleton (BinChoice t1 t2) = t1 == t2 && isRegexSingleton t1
isRegexSingleton (Repetition _ s me t) = case me of
  Nothing -> False
  Just e -> s == e && isRegexSingleton t
isRegexSingleton _ = False

--- ### Values
data Val = BoolVal Bool
         | IntVal Int
         | ResultVal String
         | RegexVal Bool RegexTree
         | Void
         deriving (Eq)

instance Show Val where
    show (BoolVal b) = show b
    show (IntVal i) = show i
    show (ResultVal r) = r
    show (RegexVal b r) = if b then "~" else "" ++ "<$0:(" ++ show r ++ ")>"
    show Void = show "VOID"

--- ### Expressions
data Exp = ValExp Val
         | VarExp String
         | AssignmentExp String Exp
--- ### Operations:
--- #### Concatenation (2)
--- #### Element-of (2)
--- #### Extract (3)
--- #### Replace (3)
--- #### ReplaceAll (3)
         | OperatorExp String Exp (Maybe Exp) (Maybe Exp)
--- #### Check (0 or 1)
--- #### Clear (0 or 1)
         | StateOpExp String (Maybe String)
         deriving (Eq)


instance Show Exp where
    show (ValExp v) = show v
    show (VarExp v) = "var " ++ show v
    show (AssignmentExp var exp) = "Set var " ++ show var ++ " to (" ++ show exp ++ ")"
    show (OperatorExp op e1 e2 e3) = "Operation " ++ show op ++ " with (" ++ opArgs
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