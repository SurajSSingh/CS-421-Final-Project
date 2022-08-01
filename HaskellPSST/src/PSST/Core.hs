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
               deriving (
            --    Eq 
            --    , 
               Show
               )

instance Eq RegexNode where
    -- Regular (mostly derived) Equality
    LiteralNode a == LiteralNode b = a == b
    ComplementNode a == ComplementNode b = a == b
    --- Choice Node Equality currently hard coded since I have no idea how to allow symmetry 
    ChoiceNode a@(ChoiceNode a1 a2) b@(ChoiceNode b1 b2) == ChoiceNode c@(ChoiceNode c1 c2) d@(ChoiceNode d1 d2)
        | a1 == c1 && a2 == c2 && b1 == d1 && b2 == d2 = True
        | a1 == c1 && a2 == c2 && b1 == d2 && b2 == d1 = True
        | a1 == c1 && a2 == d1 && b1 == c2 && b2 == d2 = True
        | a1 == c1 && a2 == d1 && b1 == d2 && b2 == c2 = True
        | a1 == c1 && a2 == d2 && b1 == c2 && b2 == d1 = True
        | a1 == c1 && a2 == d2 && b1 == d1 && b2 == c2 = True
        
        | a1 == c2 && a2 == c1 && b1 == d1 && b2 == d2 = True
        | a1 == c2 && a2 == c1 && b1 == d2 && b2 == d1 = True
        | a1 == c2 && a2 == d1 && b1 == c1 && b2 == d2 = True
        | a1 == c2 && a2 == d1 && b1 == d2 && b2 == c1 = True
        | a1 == c2 && a2 == d2 && b1 == c1 && b2 == d1 = True
        | a1 == c2 && a2 == d2 && b1 == d1 && b2 == c1 = True
        
        | a1 == d1 && a2 == c2 && b1 == c1 && b2 == d2 = True
        | a1 == d1 && a2 == c2 && b1 == d2 && b2 == c1 = True
        | a1 == d1 && a2 == c1 && b1 == c2 && b2 == d2 = True
        | a1 == d1 && a2 == c1 && b1 == d2 && b2 == c2 = True
        | a1 == d1 && a2 == d2 && b1 == c2 && b2 == c1 = True
        | a1 == d1 && a2 == d2 && b1 == c1 && b2 == c2 = True
        
        | a1 == d2 && a2 == c2 && b1 == d1 && b2 == c1 = True
        | a1 == d2 && a2 == c2 && b1 == c1 && b2 == d1 = True
        | a1 == d2 && a2 == d1 && b1 == c2 && b2 == c1 = True
        | a1 == d2 && a2 == d1 && b1 == c1 && b2 == c2 = True
        | a1 == d2 && a2 == c1 && b1 == c2 && b2 == d1 = True
        | a1 == d2 && a2 == c1 && b1 == d1 && b2 == c2 = True

    ChoiceNode a b@(ChoiceNode b1 b2) == ChoiceNode c@(ChoiceNode c1 c2) d
        | a == c1 && b1 == c2 && b2 == d = True
        | a == c1 && b1 == d && b2 == c2 = True
        | a == c2 && b1 == c1 && b2 == d = True
        | a == c2 && b1 == d && b2 == c1 = True
        | a == d && b1 == c1 && b2 == c2 = True
        | a == d && b1 == c2 && b2 == c1 = True
    ChoiceNode a b@(ChoiceNode b1 b2) == ChoiceNode c d@(ChoiceNode d1 d2) = ChoiceNode a b == ChoiceNode d c
    ChoiceNode a@(ChoiceNode a1 a2) b == ChoiceNode c@(ChoiceNode c1 c2) d = ChoiceNode b a == ChoiceNode c d
    ChoiceNode a@(ChoiceNode a1 a2) b == ChoiceNode c d@(ChoiceNode d1 d2) = ChoiceNode b a == ChoiceNode d c
    ChoiceNode a b == ChoiceNode c d
        | a == c && b == d = True
        | a == d && b == c = True
    RepetitionNode l1 s1 e1 n1 == RepetitionNode l2 s2 e2 n2 = l1 == l2 && s1 == s2 && e1 == e2 && n1 == n2
    CaptureGroupSequence _ a == CaptureGroupSequence _ b = a == b
    -- Special Generic Equality
    -- CaptureGroupSequence _ [a] == b = a == b
    -- a == CaptureGroupSequence _ [b] = a == b
    ComplementNode (ComplementNode a) == b = a == b
    a == ComplementNode (ComplementNode b) = a == b
    -- Other special cases
    ChoiceNode a b == n = a == n && b == n
    n == ChoiceNode a b = a == n && b == n
    (RepetitionNode _ 1 (Just 1) a) == b = a == b
    a == (RepetitionNode _ 1 (Just 1) b) = a == b
    a == b = False


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

--- Nothing in this specific case means Infinity
maybeLTE :: Maybe Int -> Maybe Int -> Bool
maybeLTE Nothing Nothing = True
maybeLTE (Just a) Nothing = True
maybeLTE Nothing (Just b) = False
maybeLTE (Just a) (Just b) = a <= b

--- ### Helper Regex Nodes
epsilon :: Either Bool String
epsilon = Left False
anyChar :: Either Bool String
anyChar = Left True
epsilonNode :: RegexNode
epsilonNode = LiteralNode epsilon
anyCharNode :: RegexNode
anyCharNode = LiteralNode anyChar
everyThingNode :: RegexNode
everyThingNode = RepetitionNode False 0 Nothing anyCharNode
-- #### s = single or specific
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
emptySet :: RegexNode
emptySet = CaptureGroupSequence 0 []
nullNode :: RegexNode
nullNode = emptySet
isEmpty :: RegexNode -> Bool
isEmpty n = n == emptySet

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

wrapNodeInCaptureGroup :: RegexSequence -> RegexNode
-- wrapNodeInCaptureGroup [CaptureGroupSequence num node] = CaptureGroupSequence 0 $ snd $ renumberCaptureGroup 1 node
wrapNodeInCaptureGroup n = CaptureGroupSequence 0 $ snd $ renumberCaptureGroup 1 n

hasEpsilonTransition :: RegexNode -> Bool
hasEpsilonTransition (LiteralNode (Left False)) = True
hasEpsilonTransition (ComplementNode c) = not $ hasEpsilonTransition c
hasEpsilonTransition (ChoiceNode a b) = hasEpsilonTransition a || hasEpsilonTransition b
hasEpsilonTransition (RepetitionNode _ 0 _ _) = True
hasEpsilonTransition (RepetitionNode _ _ _ n) = hasEpsilonTransition n
hasEpsilonTransition (CaptureGroupSequence _ []) = False
hasEpsilonTransition (CaptureGroupSequence _ seq) = all hasEpsilonTransition seq
hasEpsilonTransition n = False

--- ### Expressions
data Exp = IntExp Int
         | RegexExp RegexNode
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

instance Eq Diagnostic where
    UnimplementedError x == UnimplementedError y = True
    InvalidOperationError op1 == InvalidOperationError op2 = op1 == op2
    InvalidArgumentsError op1 x == InvalidArgumentsError op2 y = op1 == op2
    NumOfArgumentsError op1 expect1 given1 x  == NumOfArgumentsError op2 expect2 given2 y = op1 == op2 && expect1 == expect2 && given1 == given2
    VariableNotFoundError v1 == VariableNotFoundError v2 = v1 == v2
    x == y = False

instance Show Diagnostic where
    show (UnimplementedError x) = x ++ " Unimplemented"
    show (InvalidOperationError op) = "Operation " ++ show op ++ " does not exist"
    show (InvalidArgumentsError op args) = "Operation " ++ show op ++ " passed in invalid arguments: " ++ show args
    show (NumOfArgumentsError op expected given args) = "Operation " ++ show op ++ " passed in " ++ (if expected < given then "too many arguments, " else "too few arguments, ") ++ "given: " ++ show args
    show (VariableNotFoundError x) = "Variable " ++ show x ++ " is not defined"

unimplemented :: String -> EvalState a
unimplemented = throwError . UnimplementedError
