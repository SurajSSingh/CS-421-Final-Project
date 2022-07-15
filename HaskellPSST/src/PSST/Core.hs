module PSST.Core where
import Data.HashMap.Strict as H
import Control.Monad.State
import Control.Monad.Except


--- ### Environment
type Env = H.HashMap String [Exp]
type EvalState a = StateT Env (Except Diagnostic) a

--- ### Regex Tree
--- Nodes: Empty, epsilon, exact string, numbered capture group, repetition (lazy or greedy) from M to N?
--- Adapted from Definition 3.1 of 
data RegexTree = EmptySet
               | Epsilon
               | Literal String
               | CaptureGroup Int RegexTree
               | Sequence [RegexTree]
               | BinChoice RegexTree RegexTree
            --    | Complement RegexTree
            --    | SetChoice [RegexTree]
               | Repetition Bool Int (Maybe Int) RegexTree
               deriving (Eq
                        , Show
                        )

-- instance Show RegexTree where
--     show EmptySet = "{}"
--     show Epsilon = ""
--     show (Literal lit) = lit
--     show (Sequence seq) = concatMap show seq
--     show (BinChoice n1 n2) = show n1 ++ "|" ++ show n2
--     show (CaptureGroup num node) = "(<"++ show num ++ "> "++ show node ++ ")"
--     show (Repetition isLazy start end node) = do
--         lazySym <- if isLazy then "?" else ""
--         repeatSym <- case (start, end) of
--             (0,Just 1) -> "?"
--             (0,Nothing) -> "*"
--             (1,Nothing) -> "+"
--             (s ,Nothing) -> "{" ++ show s ++ "}"
--             (s ,Just e) -> "{" ++ show s ++ "," ++ show e ++ "}"
--         show node ++ [repeatSym] ++ [lazySym]

regexTreeSeqHelper :: [RegexTree] -> RegexTree
regexTreeSeqHelper [] = EmptySet
regexTreeSeqHelper [tree] = tree
regexTreeSeqHelper trees = Sequence trees

regexTreeRepHelper ::  Bool -> Int -> Maybe Int -> [RegexTree] -> RegexTree
regexTreeRepHelper lazy start end [] = Repetition lazy start end EmptySet
regexTreeRepHelper lazy start end [tree] = Repetition lazy start end tree
regexTreeRepHelper lazy start end (t:ts) = regexTreeRepHelper lazy start end ts
-- regexTreeRepHelper _ _ _ _ = Repetition False 0 (Just 0) EmptySet

regexTreeBuilder :: [String] -> RegexTree
regexTreeBuilder strings = fst $ regexTreeBuilderAux strings [] 1
    where
        regexTreeBuilderAux :: [String] -> [RegexTree] -> Int -> (RegexTree, (Int, [String]))
        regexTreeBuilderAux (c:cs) before num = case c of
          "|" -> (BinChoice (regexTreeSeqHelper before) $ fst $ regexTreeBuilderAux cs [] num, (num, []))
        --   "{" -> _
        --   "}" -> EmptySet
          "?" -> (rTree, (num, next))
            where
                (rTree, next) = case cs of
                    "?":n -> (regexTreeRepHelper True 0 (Just 0) before, n)
                    _ -> (regexTreeRepHelper False 0 (Just 0) before , cs)
          "+" -> (rTree, (num, next))
            where
                (rTree, next) = case cs of
                    "?":n -> (regexTreeRepHelper True 1 Nothing before, n)
                    _ -> (regexTreeRepHelper False 1 Nothing before , cs)
          "*" -> (rTree, (num, next))
            where
                (rTree, next) = case cs of
                    "?":n -> (regexTreeRepHelper True 0 Nothing before, n)
                    _ -> (regexTreeRepHelper False 0 Nothing before , cs)
          "(" -> regexTreeBuilderAux nextCs (before ++ [group]) finalNum
            where
                res = regexTreeBuilderAux cs [] (num+1)
                group = CaptureGroup num $ fst res
                (finalNum, nextCs) = snd res
          ")" -> (regexTreeSeqHelper before, (num, cs))
            -- [] -> (EmptySet, (num, cs))
            -- [b] -> (b, (num, cs))
            -- _ -> (Sequence before, (num+1, cs))
          c   -> regexTreeBuilderAux cs (before ++ [Literal c]) num

        -- regexTreeBuilderAux [] [] [a] num = (a, (num, []))
        -- regexTreeBuilderAux [] [b] num = (b, (num, []))
        -- regexTreeBuilderAux [] [] after num = (Sequence after, (num, []))
        regexTreeBuilderAux [] before num = (regexTreeSeqHelper before, (num, []))
        -- regexTreeBuilderAux [] before after num = (Sequence $ before ++ after, (num, []))
        -- regexTreeBuilderAux _ _ _ _ = (EmptySet, (-1, []))

--- ### Values
data Val = BoolVal Bool
         | IntVal Int
         | RegexVal Bool String -- RegexTree
         | Null
         deriving (Eq)

instance Show Val where
    show (BoolVal b) = show b
    show (IntVal i) = show i
    show (RegexVal b r) = "(" ++ show r ++ ")" ++ if b then "^C" else ""
    show Null = show "NULL"

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
                deriving (Eq)

instance Show Diagnostic where
    show (UnimplementedError x) = x ++ " Unimplemented"
