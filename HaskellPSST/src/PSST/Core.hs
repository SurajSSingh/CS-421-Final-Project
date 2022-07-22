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
--- Adapted from Definition 3.1
--- Nodes (from least complex to most): 
---   * Empty is not possible directly, only Nothing when Maybe Regex Tree
---   * Single Literal: Either Bool (epsilon = False, sigma/any char = True) or String
---   * Added Complement: Node accepts any string not accepted by enclosing node
---   * Capture Group: Int (0 = full string, +N = capture number, -N = extract stub number)
---   * Choice: Move down one node or the other, runs till it hits the enclosing capture group 
---   * Sequence: Recursive connection of a sequence of nodes
---   * Removed Repetition: Issue with infinite sets and whether lazy vs greedy semantics changes accepting language
type RegexSequence = [RegexNode]
data RegexNode = LiteralNode (Either Bool String)
               | ComplementNode RegexNode
               | ChoiceNode RegexNode RegexNode
               | RepetitionNode Bool Int (Maybe Int) RegexNode
               | CaptureGroupSequence Int RegexSequence
               deriving ( Eq 
               , Show
               )

-- instance Show RegexNode where
--     show (LiteralNode (Left False)) = "''"
--     show (LiteralNode (Left True)) = "."
--     show (LiteralNode (Right c)) = c
--     show (ComplementNode n) = "~" ++ show n
--     show (ChoiceNode a b) = show a ++ "|" ++ show b
--     show (RepetitionNode l s e n) = show n ++ rangeStr ++ isLazyStr
--         where
--             isLazyStr = if l then "?" else ""
--             rangeStr = case (s, e) of
--                 (start, Just end)
--                     | end == 1     -> "?"
--                     | start == end -> "{" ++ show start ++ "}"
--                     | otherwise    -> "{" ++ show start ++ "," ++ show end ++ "}"
--                 (0, Nothing)       -> "*"
--                 (1, Nothing)       -> "+"
--                 (start, Nothing)   -> "{" ++ show start ++ ",}"
--     show (CaptureGroupSequence num seq) = "<$" ++ groupNum ++ ">" ++ "(" ++ seqStr ++ ")"
--         where
--             groupNum = show $ abs num
--             seqStr = intercalate "" (Prelude.map show seq)

--- ### Helper Regex Nodes
epsilonNode :: RegexNode
epsilonNode = LiteralNode $ Left False
anyCharNode :: RegexNode
anyCharNode = LiteralNode $ Left True
-- s = single or specific
sCharNode :: String -> RegexNode
sCharNode c = LiteralNode $ Right c
kleeneStarNode :: Bool -> RegexNode -> RegexNode
kleeneStarNode l = RepetitionNode l 0 Nothing
kleenePlusNode :: Bool -> RegexNode -> RegexNode
kleenePlusNode l = RepetitionNode l 1 Nothing
optionalNode :: Bool -> RegexNode -> RegexNode
optionalNode l = RepetitionNode l 0 (Just 1)
captureGroupStub :: Int -> RegexNode
captureGroupStub n = CaptureGroupSequence (-n) []
emptySetNode :: RegexNode
emptySetNode = CaptureGroupSequence 0 []

data RegexTree = Literal (Either Bool String)
               | Complement RegexTree
               | CaptureGroup Int RegexTree
               | Choice RegexTree RegexTree
               | Sequence RegexTree RegexTree
               deriving ( Eq )

--- Helper Functions
renumberCaptureGroup :: Int -> RegexSequence -> (Int, RegexSequence)
renumberCaptureGroup num [] = (num, [])
renumberCaptureGroup num ((CaptureGroupSequence m cg):ns) | m < 0 = (finNum, (CaptureGroupSequence m newCg) : newNs)
    where
        (nextNum, newCg) = renumberCaptureGroup num cg
        (finNum, newNs) = renumberCaptureGroup nextNum ns
renumberCaptureGroup num ((CaptureGroupSequence m cg):ns) = (finNum, (CaptureGroupSequence num newCg) : newNs)
    where
        (nextNum, newCg) = renumberCaptureGroup (num+1) cg
        (finNum, newNs) = renumberCaptureGroup nextNum ns
renumberCaptureGroup num ((ComplementNode n):ns) = (finNum, (ComplementNode newN) : newNs)
    where
        (nextNum, [newN]) = renumberCaptureGroup num [n]
        (finNum, newNs) = renumberCaptureGroup nextNum ns
renumberCaptureGroup num ((ChoiceNode a b):ns) = (finNum, (ChoiceNode newA newB) : newNs)
    where
        (nextNum1, [newA]) = renumberCaptureGroup num [a]
        (nextNum2, [newB]) = renumberCaptureGroup nextNum1 [b]
        (finNum, newNs) = renumberCaptureGroup nextNum2 ns
renumberCaptureGroup num ((RepetitionNode l s e n):ns) = (finNum, (RepetitionNode l s e newN) : newNs)
    where
        (nextNum, [newN]) = renumberCaptureGroup num [n]
        (finNum, newNs) = renumberCaptureGroup nextNum ns
renumberCaptureGroup num (ln@(LiteralNode _):ns) = (finNum, ln : newNs)
    where
        (finNum, newNs) = renumberCaptureGroup num ns

-- Special Regex Tree (for convience)
epsilon :: Either Bool b
epsilon = Left False
anyCharacter :: Either Bool b
anyCharacter = Left True
-- Empty set equivalence proof: ~ ( {""} || ~({""}) ) --De Morgan's Law---> ~{""} && ~~{""} ---Double Negation--> ~{""} && {""} (Contradiction achieved)
emptySet :: RegexTree
emptySet = Complement (Choice (Literal epsilon) (Complement (Literal epsilon)))

-- isEmptySet :: RegexTree -> Bool
-- isEmptySet x | x == emptySet = True
-- isEmptySet (Complement (Choice (x) Complement (y))) = True

instance Show RegexTree where
    show (Complement (Choice a (Complement b))) | a == b = "***[[EMPTY SET]]*** => {}"
    show (Literal (Left b)) = if b then "." else ""
    show (Literal (Right c)) = if c `elem` requireEscapeRegexSymbol then "\\" ++ c else c
    show (CaptureGroup num t) = if num < 0 then "(<$" ++ show (-num) ++ ">)" else "<$" ++ show num ++ "(" ++ show t ++ ")>"
    show (Sequence t ts) = show t ++ show ts
    show (Choice t1 t2) = show t1 ++ "|" ++ show t2
    show (Complement t1) = "~" ++ show t1
    -- show EmptySet = "{}"

--- Convert a list of RegexTree into a Sequence
listToRegexTree :: [RegexTree] -> RegexTree
listToRegexTree [] = Literal epsilon -- Why epsilon over empty set? -> Empty Set accept no language, this in theory accept the empty string, which is epsilon
listToRegexTree [t] = t
listToRegexTree ((Literal epsilon):ts) = listToRegexTree ts
listToRegexTree (t:ts) = Sequence t $ listToRegexTree ts

isSubtree :: RegexTree -> RegexTree -> Bool
--- Equality between two trees always means subset
isSubtree x y | x == y = True
--- Empty Set is always a subset of any tree and never a subset of non empty trees
-- isSubtree x EmptySet = False
-- isSubtree EmptySet x = True
--- Capture Group are transparent to subset
--- Handles: */CaptureGroup (5)
--- Handles: CaptureGroup/* (4)
isSubtree x (CaptureGroup _ t) = not $ isSubtree x t
isSubtree (CaptureGroup _ t) x = not $ isSubtree t x
--- Handles: Complement/* (4) TODO
--- Handles: */Complement (3) TODO
isSubtree x (Complement t) = not $ isSubtree x t
isSubtree (Complement t) x = False
--- Handles: Literal/* (3)
---          E in E
---          A in A
---          E not in A
---          c in A
---          c in C if c == C
isSubtree (Literal x) (Literal y) = case (x, y) of
    (Left xb, Left yb) -> xb == yb
    (Left xb, Right ys) -> False
    (Right xs, Left yb) -> yb
    (Right xs, Right ys) -> xs == ys
isSubtree x@(Literal _) (Choice a b) = isSubtree x a || isSubtree x b
-- isSubtree x@(Literal _) (Sequence y Nothing) = x == y 
isSubtree x@(Literal _) (Sequence y ys) = False

--- Handles: Choice/* (3)
isSubtree (Choice a b) l@(Literal _) = isSubtree a l && isSubtree b l
isSubtree (Choice a b) (Choice c d) = (isSubtree a c && isSubtree b d) || (isSubtree a d && isSubtree b c)
-- isSubtree (Choice a b) (Sequence x Nothing) = isSubtree a x && isSubtree b x
isSubtree (Choice a b) s@(Sequence x xs) = isSubtree a s || isSubtree b s

--- Handles: Sequence/* (3)
-- isSubtree (Sequence x Nothing) l@(Literal _) = isSubtree x l
isSubtree (Sequence x mx) l@(Literal _) = False
-- isSubtree (Sequence x Nothing) (Choice a b) = isSubtree x a || isSubtree x b
isSubtree (Sequence x mx) (Choice c d) = False
-- isSubtree (Sequence x Nothing) (Sequence y Nothing) = isSubtree x y 
-- isSubtree (Sequence x mx) (Sequence y Nothing) = False 
-- isSubtree (Sequence x Nothing) (Sequence y ys) = False 
isSubtree (Sequence x xs) (Sequence y ys) = False


--- ### Expressions
data Exp = IntExp Int
         | RegexExp RegexTree
         | VarExp String
         | ResultValExp String
         | AssignmentExp String Exp
         | OperatorExp String Exp (Maybe Exp) (Maybe Exp)
         | StateOpExp String (Maybe Exp)
         deriving (Eq)

instance Show Exp where
    show (IntExp i) = show i
    show (RegexExp r) = show r
    show (VarExp v) = "var " ++ show v
    show (ResultValExp res) = res
    show (AssignmentExp var exp) = "Set var " ++ show var ++ " to (" ++ show exp ++ ")"
    show (OperatorExp op e1 e2 e3) = "Op " ++ show op ++ " with (" ++ opArgs
        where
            opArgs = show e1 ++ (case e2 of
               Nothing -> ")"
               Just v2 -> " and " ++ show v2 ++ (case e3 of
                  Nothing -> ")"
                  Just v3 -> " and " ++ show v3 ++ ")"))
    show (StateOpExp op exp) = case exp of
      Nothing -> "Running " ++ op ++ " on current environment"
      Just s -> "Running " ++ op ++ " on " ++ show s

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
