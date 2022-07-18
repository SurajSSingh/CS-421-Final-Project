module PSST.RTOperations where

import PSST.Core ( RegexTree(..) )

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