module PSST.RTOperations where

import PSST.Core ( RegexTree(..) )

--- Nothing in this specific case means Infinity
maybeLTE :: Maybe Int -> Maybe Int -> Bool
maybeLTE Nothing Nothing = True
maybeLTE (Just a) Nothing = True
maybeLTE Nothing (Just b) = False
maybeLTE (Just a) (Just b) = a <= b

--- Update a capture group's number
updateCaptureGroupNumber :: RegexTree -> Int -> (RegexTree, Int)
updateCaptureGroupNumber (CaptureGroup cgNum t) num = (CaptureGroup num newt, newNum)
  where
    (newt, newNum) = updateCaptureGroupNumber t (num+1)
updateCaptureGroupNumber (Sequence ts) num = (Sequence [], newNum)
  where
    (newSeq, newNum) = foldr aux ([], num) ts
      where
        aux t acc = (ut : fst acc, nexNum)
          where
            (ut, nexNum) = updateCaptureGroupNumber t $ snd acc
updateCaptureGroupNumber (BinChoice t1 t2) num = (BinChoice ut1 ut2, n2)
  where
    (ut1, n1) = updateCaptureGroupNumber t1 num
    (ut2, n2) = updateCaptureGroupNumber t2 n1
updateCaptureGroupNumber (Repetition l s me t) num = (Repetition l s me ut, newNum)
  where
    (ut, newNum) = updateCaptureGroupNumber t num
updateCaptureGroupNumber t num = (t, num)

--- ### Regex Tree pattern operations
--- #### Regex Union: Combines two regex trees with binary choice and updating capture group numbers
regexTreeUnion :: RegexTree -> RegexTree -> RegexTree
regexTreeUnion t1 t2 = BinChoice (CaptureGroup 1 ut1) (CaptureGroup newNum ut2)
  where
    (ut1, newNum) = updateCaptureGroupNumber t1 2
    (ut2, finNum) = updateCaptureGroupNumber t2 (newNum + 1)

--- #### Does a given regex tree contain only a single valid string 
---      These are:
---       * Any Literal, except AnyCharLiteral, including Epsilon (i.e. {""})
---       * A sequence with one value that is itself a singleton, example: {"abc"}
---       * A capture group that has a singleton
---       * A binary choice where both branches are the same and the branches are singleton
---       * Repetition where the start and end match and the tree is a singleton itself
---     All else are not singletons (either empty or multiple)
isRegexSingleton :: RegexTree -> Bool
isRegexSingleton Epsilon = True
isRegexSingleton (Literal _) = True
isRegexSingleton (Sequence []) = False
isRegexSingleton (Sequence ts) = all isRegexSingleton ts
isRegexSingleton (CaptureGroup _ t) = isRegexSingleton t
isRegexSingleton (BinChoice t1 t2) = t1 == t2 && isRegexSingleton t1
isRegexSingleton (Repetition _ s me t) = case me of
  Nothing -> False
  Just e -> s == e && isRegexSingleton t
isRegexSingleton _ = False

--- #### Is a given regex tree a subset of another regex tree
---      Recall that strings are sets in this language, e.g. "a" means {"a"}
---      Above each case will show how many cases it should match: (sub/sup (no. of cases))
---      Total = 81
isRegexSubLang :: RegexTree -> RegexTree -> Bool
--- Handle: EmptySet/* (9) 
isRegexSubLang EmptySet t = True
--- Handle: R1/R2 where type of R1 == type of R2 (1 + 1 + 6 = 8)
isRegexSubLang t1@(CaptureGroup _ n1) t2@(CaptureGroup _ n2) = isRegexSubLang n1 n2
isRegexSubLang (Repetition _ s1 me1 t1) (Repetition _ s2 me2 t2) = s2 <= s1 && maybeLTE me1 me2 && isRegexSubLang t1 t2
isRegexSubLang t1 t2 | t1 == t2 = True
--- Handle: CaptureGroup/* and */CaptureGroup (7 + 7)
isRegexSubLang (CaptureGroup _ cg) t = isRegexSubLang cg t
isRegexSubLang t (CaptureGroup _ cg) = isRegexSubLang t cg
--- Handle: Epsilon/(Sequence, Repetition) (2)
---   * Epsilon == []
isRegexSubLang Epsilon (Sequence []) = True
isRegexSubLang (Sequence []) Epsilon = True
---   * Epsilon == T{0, X} (T is any regexTree, X nat up to inf)
isRegexSubLang Epsilon (Repetition _ s _ t) = s == 0 && t /= EmptySet
--- Handle: */Sequence (6)
isRegexSubLang t (Sequence []) = False
---   * Single Sequence
isRegexSubLang (Sequence [x]) (Sequence [y]) = isRegexSubLang x y
isRegexSubLang t (Sequence [s]) = isRegexSubLang t s
---   * Multiple in Sequence
isRegexSubLang (Sequence xs) (Sequence ys) = all (uncurry isRegexSubLang)  (zip xs ys)
isRegexSubLang t (Sequence (x:xs)) = t == x                       --- TODO: How to subset a list
--- Handle: */Repetition (6)
isRegexSubLang t1 (Repetition _ s2 me2 t2) = 0 < s2 && isRegexSubLang t1 t2  -- At least one of subset
--- Handle: */BinChoice (5)
isRegexSubLang b1@(BinChoice s1 s2) b2@(BinChoice t1 t2)
  | s1 `isRegexSubLang` s2 = isRegexSubLang s2 t1 || isRegexSubLang s2 t2
  | s2 `isRegexSubLang` s1 = isRegexSubLang s1 t1 || isRegexSubLang s1 t2
  | otherwise = (isRegexSubLang s1 t1 && isRegexSubLang s2 t2) || (isRegexSubLang s2 t1 && isRegexSubLang s1 t2)
isRegexSubLang t (BinChoice t1 t2) = isRegexSubLang t t1 || isRegexSubLang t t2

--- Handles Literal/AnyChar (1)
isRegexSubLang (Literal _) AnyCharLiteral = True

--- Handle: */CaptureGroupStub (7)
--- Handle: AnyChar/* (6)
--- Handle: Literal/* (6)
--- Handle: Epsilon/* (5)
--- Handle: Sequence/Epsilon (1)
isRegexSubLang _ _ = False

--- #### Regex Unify: Return a regex tree that unifies two trees to their common pattern
--- ####              , will be empty set if cannot be unified
regexTreeUnify :: RegexTree -> RegexTree -> RegexTree
--- Sub-language case, use the sub language as unification
regexTreeUnify t1 t2
  | t1 `isRegexSubLang` t2 = t1
  | t2 `isRegexSubLang` t1 = t2
--- Empty set: Choose which ever is not empty
regexTreeUnify t1 EmptySet = t1
regexTreeUnify EmptySet t2 = t2
--- Capture Group: Referentially transparent to unification
---                ex: ab, a(b), (a)b, (ab) can all be unified into ab
regexTreeUnify t1 (CaptureGroup _ t2) = regexTreeUnify t1 t2
regexTreeUnify (CaptureGroup _ t1) t2 = regexTreeUnify t1 t2
--- Epsilons: Only epsilon can merge with other epsilons
---           Rest are in final catch-all case
regexTreeUnify Epsilon Epsilon = Epsilon
--- Literals and Any Literal:
--- Literals must match exactly, or match the any character literal
regexTreeUnify t1@(Literal l1) t2@(Literal l2)
    | l1 == l2 = t1
regexTreeUnify AnyCharLiteral t2@(Literal l) = t2
regexTreeUnify t1@(Literal l) AnyCharLiteral = t1

--- Choice: If it's between two binary choices, unify each choice with each choice of the other.
---         Then the branches are checked for empty, returning either another unified binary choice or a single branch.
---         If it's an other structure, it will try to unify with each branch, then unify the resulting branches
regexTreeUnify (BinChoice t1a t1b) (BinChoice t2a t2b) = case (regexTreeUnify (regexTreeUnify t1a t2a) (regexTreeUnify t1a t2b), regexTreeUnify (regexTreeUnify t1b t2a) (regexTreeUnify t1b t2b)) of
    (EmptySet, t) -> t
    (t, EmptySet) -> t
    (nt1, nt2) -> case regexTreeUnify nt1 nt2 of
      EmptySet -> BinChoice nt1 nt2
      t -> t

regexTreeUnify t1 (BinChoice t2a t2b) = regexTreeUnify (regexTreeUnify t1 t2a) (regexTreeUnify t1 t2b)
regexTreeUnify (BinChoice t1a t1b) t2 = regexTreeUnify (regexTreeUnify t1a t2) (regexTreeUnify t1b t2)

--- Repetition: TODO
regexTreeUnify (Repetition l1 s1 me1 t1) (Repetition l2 s2 me2 t2) | t1 == t2 = t2
regexTreeUnify t1 (Repetition l s me t2) | t1 == t2 = t2
regexTreeUnify (Repetition l s me t1) t2 | t1 == t2 = t2

--- Sequences: TODO
regexTreeUnify (Sequence s1) (Sequence s2) = Sequence []
regexTreeUnify (Sequence s) t1 = Sequence []

--- Otherwise, if it can't be unified, return empty set
regexTreeUnify _ _ = EmptySet
