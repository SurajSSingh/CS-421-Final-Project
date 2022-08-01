module PSST.RTOperations where

import PSST.Core ( RegexNode (..), epsilonNode, emptySet, wrapNodeInCaptureGroup, everyThingNode, optionalNode, anyCharNode, epsilon, anyChar, maybeLTE, isEmpty, RegexSequence )

-- # Regex Node (originally Regex Tree) operations functions

--- ## General Helper Functions
maybeMin :: Maybe Int -> Maybe Int -> Maybe Int
maybeMin Nothing Nothing = Nothing
maybeMin Nothing (Just y) = Just y
maybeMin (Just x) Nothing = Just x
maybeMin (Just x) (Just y)
    | x <= y = Just x
    | otherwise = Just y

maybeMax :: Maybe Int -> Maybe Int -> Maybe Int
maybeMax Nothing Nothing = Nothing
maybeMax Nothing (Just y) = Nothing
maybeMax (Just x) Nothing = Nothing
maybeMax (Just x) (Just y)
    | x >= y = Just x
    | otherwise = Just y

hasEpsilonTransition :: RegexNode -> Bool
hasEpsilonTransition (LiteralNode (Left False)) = True
hasEpsilonTransition (ComplementNode c) = not $ hasEpsilonTransition c
hasEpsilonTransition (ChoiceNode a b) = hasEpsilonTransition a || hasEpsilonTransition b
hasEpsilonTransition (RepetitionNode _ 0 _ _) = True
hasEpsilonTransition (RepetitionNode _ _ _ n) = hasEpsilonTransition n
hasEpsilonTransition (CaptureGroupSequence _ []) = False
hasEpsilonTransition (CaptureGroupSequence _ seq) = all hasEpsilonTransition seq
hasEpsilonTransition n = False

--- ## Regex Tree Singleton: Does a given regex tree contain only a single valid string 
---    These are:
---     * Any Literal, except AnyCharLiteral, including Epsilon (i.e. {""})
---     * A double complement that has a singleton
---         * Single complement can never be singleton since it's inner value would need to have nearly infinite values 
---     * A sequence with one value that is itself a singleton, example: {"abc"}
---     * A binary choice where both branches are the same and the branches are singleton
---     * Repetition where the start and end match and the tree is a singleton itself
---     * A capture group that has a singleton
---   All else are not singletons (either empty or multiple)
isNodeSingleton :: RegexNode -> Bool
isNodeSingleton (LiteralNode n)
    | n == Left True = False
    | otherwise = True
isNodeSingleton (ComplementNode (ComplementNode c)) = isNodeSingleton c
isNodeSingleton (ComplementNode c) = False -- Why? The only way to have a singleton complement is having every value except a literal, which is handled in previous case
isNodeSingleton (ChoiceNode a b)
    | a == b = isNodeSingleton a
    | otherwise = False
isNodeSingleton (RepetitionNode _ s (Just e) n)
    | s == e = isNodeSingleton n
    | otherwise = False
isNodeSingleton (RepetitionNode _ s Nothing n)
    | n == epsilonNode = True
    | otherwise = False
isNodeSingleton (CaptureGroupSequence _ n) = all isNodeSingleton n


subNodeSequence :: RegexSequence -> RegexSequence -> Bool
subNodeSequence [] [] = True
subNodeSequence [] (b:bs)
    | hasEpsilonTransition b = subNodeSequence [] bs
    | otherwise = False
subNodeSequence (a:as) []
    | hasEpsilonTransition a = subNodeSequence as []
    | otherwise = False
--- Sequences: Try to run subsequence as normal, if failed bring them up to the current level
subNodeSequence (a@(CaptureGroupSequence _ sa):as) (b@(CaptureGroupSequence _ sb):bs) = (a `isSubNode` b && subNodeSequence as bs) || subNodeSequence (sa++as) (sb++bs)
subNodeSequence (a@(CaptureGroupSequence _ seq):as) (b:bs) = (a `isSubNode` b && subNodeSequence as bs) || subNodeSequence (seq++as) (b:bs)
subNodeSequence (a:as) (b@(CaptureGroupSequence _ seq):bs) = (a `isSubNode` b && subNodeSequence as bs) || subNodeSequence (a:as) (seq++bs)
--- Choice: Split paths
subNodeSequence ((ChoiceNode c d):as) bs = subNodeSequence (c:as) bs && subNodeSequence (d:as) bs
subNodeSequence as ((ChoiceNode c d):bs) =  subNodeSequence as (c:bs) || subNodeSequence as (d:bs)
--- Repetition cases:
---     * a sub node of n -> reduce repetition by 1 if start above 0, else:
---       * lazy   -> try early end, come back to do remaining if failed
---       * greedy -> try later end, come back to do less if failed
---     * a not sub node of n and n not an epsilon transition -> fail
---     * 3 sub-cases:
---       1. Both repeat
---       2. Repeated Capture Group
---       3. Generic Repeated
subNodeSequence ((RepetitionNode l1 s1 me1 n1):as) ((RepetitionNode l2 s2 me2 n2):bs)
    | s1 >= s2 && me1 `maybeLTE` me2 = n1 `isSubNode` n2 && subNodeSequence as bs

subNodeSequence (a:as) ((RepetitionNode lazy 0 Nothing cg@(CaptureGroupSequence _ n)):bs) = if lazy
    then subNodeSequence (a:as) bs || ((a `isSubNode` cg || subNodeSequence (a:as) n)  && subNodeSequence as ((RepetitionNode lazy 0 Nothing cg):bs))
    else ((a `isSubNode` cg || subNodeSequence (a:as) n)  && subNodeSequence as ((RepetitionNode lazy 0 Nothing cg):bs)) || subNodeSequence (a:as) bs
subNodeSequence (a:as) ((RepetitionNode lazy s Nothing cg@(CaptureGroupSequence _ n)):bs) = a `isSubNode` cg && subNodeSequence as ((RepetitionNode lazy (s-1) Nothing cg):bs)
subNodeSequence (a:as) ((RepetitionNode lazy 0 (Just 0) cg@(CaptureGroupSequence _ _)):bs) = subNodeSequence (a:as) bs
subNodeSequence (a:as) ((RepetitionNode lazy 0 (Just e) cg@(CaptureGroupSequence _ n)):bs) = if lazy
    then subNodeSequence (a:as) bs || ((a `isSubNode` cg || subNodeSequence (a:as) n)  && subNodeSequence as ((RepetitionNode lazy 0 (Just $ e-1) cg):bs))
    else ((a `isSubNode` cg || subNodeSequence (a:as) n)  && subNodeSequence as ((RepetitionNode lazy 0 (Just $ e-1) cg):bs)) || subNodeSequence (a:as) bs
subNodeSequence (a:as) ((RepetitionNode lazy s (Just e) cg@(CaptureGroupSequence _ _)):bs) = a `isSubNode` cg && subNodeSequence as ((RepetitionNode lazy (s-1) (Just $ e-1) cg):bs)

subNodeSequence (a:as) ((RepetitionNode lazy 0 Nothing n):bs) = if lazy
    then subNodeSequence (a:as) bs || (a `isSubNode` n && subNodeSequence as ((RepetitionNode lazy 0 Nothing n):bs))
    else (a `isSubNode` n && subNodeSequence as ((RepetitionNode lazy 0 Nothing n):bs)) || subNodeSequence (a:as) bs
subNodeSequence (a:as) ((RepetitionNode lazy s Nothing n):bs) = a `isSubNode` n && subNodeSequence as ((RepetitionNode lazy (s-1) Nothing n):bs)
subNodeSequence (a:as) ((RepetitionNode lazy 0 (Just 0) n):bs) = subNodeSequence (a:as) bs
subNodeSequence (a:as) ((RepetitionNode lazy 0 (Just e) n):bs) = if lazy
    then subNodeSequence (a:as) bs || (a `isSubNode` n && subNodeSequence as ((RepetitionNode lazy 0 (Just $ e-1) n):bs))
    else (a `isSubNode` n && subNodeSequence as ((RepetitionNode lazy 0 (Just $ e-1) n):bs)) || subNodeSequence (a:as) bs
subNodeSequence (a:as) ((RepetitionNode lazy s (Just e) n):bs) = a `isSubNode` n && subNodeSequence as ((RepetitionNode lazy (s-1) (Just $ e-1) n):bs)
--- Default includes: Literal, Complement
subNodeSequence (a:as) (b:bs) = a `isSubNode` b && subNodeSequence as bs

--- ## isSubNode/isSubset: Is a given node 1 a subnode (language subset) of node 2.
isSubNode :: RegexNode -> RegexNode -> Bool
--- ### Logical subnodes
--- #### Equality is always true
isSubNode a b | a == b = True
--- #### Empty set is always subset
isSubNode (CaptureGroupSequence _ []) n = True
--- #### Empty set is never superset for non-empty sets
isSubNode n (CaptureGroupSequence _ []) = isEmpty n

--- ### Same Type
--- #### Literal:
--- ##### Non-epsilon subset of any char -> True
--- ##### all else -> False
isSubNode (LiteralNode a) (LiteralNode b)
    | a /= epsilon && b == anyChar = True
    | otherwise = False
--- #### Complement: Contrapositive -> ~a subset ~b == b subset a
isSubNode (ComplementNode a) (ComplementNode b) = b `isSubNode` a
--- #### Choice: a|b subset c|d -> (a subset c or d) and (b subset c or d)
isSubNode (ChoiceNode a b) (ChoiceNode c d) = (a `isSubNode` c || a `isSubNode` d) && (b `isSubNode` c || b `isSubNode` d)
--- #### Repeat: r1's range is within r2's range and r1's node subset r2's node
isSubNode (RepetitionNode l1 s1 Nothing r1) (RepetitionNode l2 s2 Nothing r2)
    | s1 >= s2 = r1 `isSubNode` r2
    | otherwise = False
isSubNode (RepetitionNode l1 s1 (Just e1) r1) (RepetitionNode l2 s2 Nothing r2)
    | s1 >= s2 = r1 `isSubNode` r2
    | otherwise = False
isSubNode (RepetitionNode l1 s1 (Just e1) r1) (RepetitionNode l2 s2 (Just e2) r2)
    | s1 >= s2 && e1 <= e2 = r1 `isSubNode` r2
    | otherwise = False
--- #### (Non-empty) Sequence
isSubNode (CaptureGroupSequence _ seq1) (CaptureGroupSequence _ seq2) = subNodeSequence seq1 seq2

-- *** All below are non-equal, non-empty, and different types ***
--- *** AnyNode = any OTHER node type ***

--- ### (Single) Group -> remove group from comparison
isSubNode n (CaptureGroupSequence _ [s]) = n `isSubNode` s
isSubNode (CaptureGroupSequence _ [s]) n = s `isSubNode` n

--- ### AnyNode subset Complement -> True if any node is not subset of complement's inner node
isSubNode n (ComplementNode c) = not $ n `isSubNode` c
--- Complement subset AnyNode -> ?? (maybe always False since Complements are usually infinite)
isSubNode (ComplementNode c) node = False


--- ### Literal with (AnyNode - Complement)
--- #### Literal subset Choice
isSubNode ln@(LiteralNode _) (ChoiceNode a b) = ln `isSubNode` a || ln `isSubNode` b
--- #### Literal subset Repetition
--- ##### Epsilon subset Repetition
isSubNode ln@(LiteralNode (Left False)) (RepetitionNode l s e n)
    | s == 0 = True
    | otherwise = ln == n
--- ##### Non-epsilon subset Repetition
isSubNode ln@(LiteralNode _) (RepetitionNode l s e n)
    | s <= 1 = ln `isSubNode` n
    | otherwise = False
--- #### Literal subset (Multi-Node) Sequence
isSubNode ln@(LiteralNode _) cg@(CaptureGroupSequence _ seq)
    | ln == epsilonNode = hasEpsilonTransition cg
    | otherwise = False

--- ### Choice with (AnyNode)
--- #### Choice subset Literal
--- #### Choice subset Repetition
--- #### Choice subset Group
--- #### Choice subset Complement
isSubNode (ChoiceNode a b) otherNode = a `isSubNode` otherNode && b `isSubNode` otherNode


--- ### Repeat with (AnyNode)
--- #### Repeat subset Literal
isSubNode rn@(RepetitionNode l 0 (Just 0) n) ln@(LiteralNode (Left False)) = True
isSubNode rn@(RepetitionNode l 1 (Just 1) n) ln@(LiteralNode _) = n `isSubNode` ln
isSubNode rn@(RepetitionNode l s e n) ln@(LiteralNode _) = False
--- #### Repeat subset Choice
isSubNode rn@(RepetitionNode l s e n) (ChoiceNode a b) = rn `isSubNode` a || rn `isSubNode` b
--- #### Repeat subset Group
--- ##### Levels:
---              * rn subset cg: What we are comparing already (need to go lower)
---              * rn subset a: Since 'a' fully consumes 'rn' but there is also 'as', if 'as' is not all epsilon transitions, then False
---              * n subset cg: Check if n can reduce cg
---              * n subset a: Check if n subset of a
isSubNode rn@(RepetitionNode l s e n) cg@(CaptureGroupSequence num (a:as))
  | rn `isSubNode` a = hasEpsilonTransition (CaptureGroupSequence num as)
  | n `isSubNode` cg = False -- FIXME
  | n `isSubNode` a = case e of
                    Nothing -> RepetitionNode l (max 0 (s-1)) e n `isSubNode` CaptureGroupSequence num as
                    Just i -> RepetitionNode l (max 0 (s-1)) (Just (max 0 (i-1))) n `isSubNode` CaptureGroupSequence num as
  | otherwise = False


--- ### (Multi-Node) Sequence subset AnyNode
--- #### Group subset Literal 
--- ##### Group subset Epsilon -> True if group has all epsilon transitions
--- ##### Group subset Non-epsilon -> False
isSubNode cg@(CaptureGroupSequence _ (x:y:seq)) (LiteralNode (Left False)) = hasEpsilonTransition cg
isSubNode cg@(CaptureGroupSequence _ (x:y:seq)) (LiteralNode _) = False
--- #### Group subset Choice -> group subset of left OR right side of choice (see Choice section)
isSubNode cg@(CaptureGroupSequence _ (x:y:seq)) (ChoiceNode a b) = cg `isSubNode` a || cg `isSubNode` b
--- #### Group subset Repeat -> Each node in group is subset of repeat node and repeat node's end is bigger than length of sequence
isSubNode cgs@(CaptureGroupSequence _ (x:y:zs)) (RepetitionNode _ s Nothing r)
    | s <= 1 = cgs `isSubNode` r
    | otherwise = False
isSubNode (CaptureGroupSequence _ seq@(x:y:zs)) (RepetitionNode _ _ (Just end) rn) = subNodeSeqRepAux seq end rn
    where
        subNodeSeqRepAux [] _ _ = True
        subNodeSeqRepAux (x:xs) 0 _ = False
        subNodeSeqRepAux (x:xs) e n = x `isSubNode` n && subNodeSeqRepAux xs (e-1) n

--- ### Non-Literal subset Literal (Handled in their own respective sections)
--- #### Choice subset Literal -> Both side must be subset of Literal
--- #### Repeat subset Literal -> Repeat only once and it's node is subset of Literal
--- #### Group subset Literal -> Single sequence group whose node is subset of Literal
--- #### Complement subset Literal -> ???

--- ### AnyNode subset Choice -> AnyNode subset of left side OR subset of right side
--- ### Choice subset AnyNode -> Choice subset of left side AND subset of right side

--- ### Repeat subset Repeat -> If repeat 1's range is inside the range of repeat 2 and repeat 1's node is subset of repeat 2's node
--- ### AnyNode subset Repeat
--- #### Epsilon subset Repeat -> True if repeat starts at 0

--- #### Node subset Repeat -> True if repeat starts at most 1 and subset of repeat's node 
isSubNode n (RepetitionNode _ s me r)
    | s <= 1 = case me of
        Just 0 -> False
        _ -> n `isSubNode` r
--- ### Repeat subset AnyNode -> True if repeats only once and it's inner node is subset of other node

--- ### DEFAULT: False
isSubNode _ _ = False


--- ## Regex Node Union: Take the set union of two regex trees.
---                      At minimum, a union can occur by creating Choice on both trees
regexUnion :: RegexNode -> RegexNode -> RegexNode
--- ### Equality or subnode can just use larger of two
--- #### Includes Special Case: Empty Set -> return node
regexUnion x y
    | x `isSubNode` y = y
    | y `isSubNode` x = x

--- #### AnyChar | Epsilon -> optional any char
regexUnion (LiteralNode (Left False)) (LiteralNode (Left True)) = wrapNodeInCaptureGroup [optionalNode False anyCharNode]
regexUnion (LiteralNode (Left True)) (LiteralNode (Left False)) = wrapNodeInCaptureGroup [optionalNode False anyCharNode]
--- #### LitChar | Epsilon -> optional literal
regexUnion (LiteralNode (Left False)) ln@(LiteralNode (Right c)) = wrapNodeInCaptureGroup [optionalNode False ln]
regexUnion ln@(LiteralNode (Right c)) (LiteralNode (Left False)) = wrapNodeInCaptureGroup [optionalNode False ln]
--- #### Epsilon | Complement -> use DEFAULT
--- #### Epsilon | Choice -> optional choice
regexUnion (LiteralNode (Left False)) cn@(ChoiceNode a b) = wrapNodeInCaptureGroup [optionalNode False cn]
regexUnion cn@(ChoiceNode a b) (LiteralNode (Left False)) = wrapNodeInCaptureGroup [optionalNode False cn]
--- #### Epsilon | Repetition{1,e?} -> repeat*
regexUnion (LiteralNode (Left False)) (RepetitionNode l 1 e n) = wrapNodeInCaptureGroup [RepetitionNode l 0 e n]
regexUnion (RepetitionNode l 1 e n) (LiteralNode (Left False)) = wrapNodeInCaptureGroup [RepetitionNode l 0 e n]
--- #### Epsilon | Repetition{2+,e?} -> "" | repeat
regexUnion (LiteralNode (Left False)) (RepetitionNode l s e n) = wrapNodeInCaptureGroup [ChoiceNode epsilonNode $ RepetitionNode l 0 e n]
regexUnion (RepetitionNode l s e n) (LiteralNode (Left False)) = wrapNodeInCaptureGroup [ChoiceNode epsilonNode $ RepetitionNode l 0 e n]
--- #### Epsilon | Group -> optional group
regexUnion (LiteralNode (Left True)) cg@(CaptureGroupSequence _ _) = wrapNodeInCaptureGroup [optionalNode False cg]
regexUnion cg@(CaptureGroupSequence _ _) (LiteralNode (Left True)) = wrapNodeInCaptureGroup [optionalNode False cg]

--- *** Every Node below is non-equal, non-subsets, non-empty, non-epsilon ***
--- ### Literal | Literal

--- #### LitChar | AnyChar -> any char
regexUnion (LiteralNode (Left True)) ln@(LiteralNode (Right c)) = wrapNodeInCaptureGroup [anyCharNode]
regexUnion ln@(LiteralNode (Right c)) (LiteralNode (Left True)) = wrapNodeInCaptureGroup [anyCharNode]
--- #### LitChar | LitChar -> l1 | l2
--- ### Literal | Choice


--- ### Choice
regexUnion (ChoiceNode a b) n
    | a `isSubNode` n && b `isSubNode` n = n
    | a `isSubNode` n = wrapNodeInCaptureGroup[ChoiceNode n b]
    | b `isSubNode` n = wrapNodeInCaptureGroup[ChoiceNode a n]
    | n `isSubNode` a || n `isSubNode` b = wrapNodeInCaptureGroup[ChoiceNode a b]
    | otherwise = wrapNodeInCaptureGroup[ChoiceNode a (ChoiceNode b n)]
regexUnion n (ChoiceNode a b)
    | a `isSubNode` n && b `isSubNode` n = n
    | a `isSubNode` n = wrapNodeInCaptureGroup [ChoiceNode n b]
    | b `isSubNode` n = wrapNodeInCaptureGroup [ChoiceNode a n]
    | n `isSubNode` a || n `isSubNode` b = wrapNodeInCaptureGroup [ChoiceNode a b]
    | otherwise = wrapNodeInCaptureGroup [ChoiceNode a (ChoiceNode b n)]
--- #### AnyChar/LitCar | Choice
-- regexUnion ln@(LiteralNode _) (ChoiceNode a b)
--     | a `isSubNode` ln && b `isSubNode` ln = ln
--     | a `isSubNode` ln = ChoiceNode ln b
--     | b `isSubNode` ln = ChoiceNode a ln
--     | ln `isSubNode` a || ln `isSubNode` b = ChoiceNode a b
--     | otherwise = ChoiceNode a (ChoiceNode b ln)
-- regexUnion (ChoiceNode a b) ln@(LiteralNode _)
--     | a `isSubNode` ln && b `isSubNode` ln = ln
--     | a `isSubNode` ln = ChoiceNode ln b
--     | b `isSubNode` ln = ChoiceNode a ln
--     | otherwise = ChoiceNode a (ChoiceNode b ln)
--- ### Literal | Repetition

--- #### AnyChar | Repetition -> . | repeat
--- #### LitChar | Repetition -> c | repeat
--- ### Literal | Group

--- #### AnyChar | Group -> any | group
--- #### LitChar | Group -> c | group
--- ### Choice | Choice -> c1 | c2
--- ### Choice | Repetition
-- regexUnion rn@(RepetitionNode l s e n) (ChoiceNode a b) = _
-- regexUnion (ChoiceNode a b) rn@(RepetitionNode l s e n) = _
--- ### Choice | Group -> c1 | group
--- ### Repetition | Repetition 
--- #### Repetition subnode ranged 
regexUnion rn1@(RepetitionNode l1 s1 me1 n1) rn2@(RepetitionNode l2 s2 me2 n2)
    | n1 `isSubNode` n2 = wrapNodeInCaptureGroup [RepetitionNode (l1 || l2) (min s1 s2) (maybeMax me1 me2) n2]
    | n2 `isSubNode` n1 = wrapNodeInCaptureGroup [RepetitionNode (l1 || l2) (min s1 s2) (maybeMax me1 me2) n1]
--- #### All other repeat -> rep1 | rep2
--- ### Repetition | Group -> rep | group
--- ### Group | Group
--- #### Single Group | Single Group -> union inner1 inner2
regexUnion (CaptureGroupSequence _ [a]) (CaptureGroupSequence _ [b]) = regexUnion a b

--- ### Any/Complement when any is complemented -> everything
regexUnion x (ComplementNode c) | x == c = wrapNodeInCaptureGroup [everyThingNode]
regexUnion (ComplementNode c) x | x == c = wrapNodeInCaptureGroup [everyThingNode]
--- ### DEFAULT: a b = ChoiceNode a b
regexUnion x y = wrapNodeInCaptureGroup[ChoiceNode x y]




--- ## Regex Node Unify: Take the set intersection of two regex trees.
---                      If no unification can occur, then return the empty set 
---                      (one of the places where an empty set can be introduced in the program)              
regexUnify :: RegexNode -> RegexNode -> RegexNode
--- ### Equality or subnode can just use smaller for two
--- #### Includes Special Case: Empty Set -> return empty set
regexUnify x y
    | x `isSubNode` y = x
    | y `isSubNode` x = y
--- *** Every Node below is non-equal, non-subsets, non-empty ***
--- #### Single Group | Single Group -> unify inner1 inner2
regexUnify (CaptureGroupSequence _ [a]) (CaptureGroupSequence _ [b]) = regexUnify a b
--- ### Literal & Literal
--- #### Epsilon & AnyChar -> empty set (any char is only non-empty character)
--- #### Epsilon & LitChar -> empty set
--- #### LitChar & AnyChar -> char
regexUnify (LiteralNode (Left True)) ln@(LiteralNode (Right c)) = wrapNodeInCaptureGroup [ln]
regexUnify ln@(LiteralNode (Right c)) (LiteralNode (Left True)) = wrapNodeInCaptureGroup [ln]
--- ### Literal & Choice
--- #### Epsilon & Choice -> empty set
--- #### AnyChar & Choice -> 
--- #### LitChar & Choice -> empty set
--- ### Literal & Repeat
--- #### Epsilon & Repeat{0,} -> epsilon
--- #### Epsilon & Repeat{1+,} -> empty set
--- #### AnyChar & Repeat
--- #### LitChar & Repeat -> empty set
--- ### Literal & Group
--- #### Epsilon & Group -> empty set
--- #### AnyChar & Group -> 
--- #### LitChar & Group -> empty set

--- ### Choice & Choice 
--- #### (single match) a|b & c|b -> b
--- #### *** double match is equality ***
--- #### (no match) a|b & c|d -> empty set
regexUnify (ChoiceNode a b) (ChoiceNode c d)
    | a == c = wrapNodeInCaptureGroup[a]
    | a == d = wrapNodeInCaptureGroup[a]
    | b == c = wrapNodeInCaptureGroup[b]
    | b == d = wrapNodeInCaptureGroup[b]
    | otherwise = emptySet
--- ### Choice & Repeat (use distribution)
--- #### (repeat at most once) a|b & a{s,(x>1)} -> a&a{s,(x>1)} | b&a{s,(x>1)}
regexUnify (ChoiceNode a b) (RepetitionNode l s (Just e) n)
    | s <= 1 && e >= 1 = regexUnion (regexUnify a n) (regexUnify b n)
regexUnify (RepetitionNode l s (Just e) n) (ChoiceNode a b)
    | s <= 1 && e >= 1 = regexUnion (regexUnify a n) (regexUnify b n)
--- #### (repeat at least once) a|b & a{1,e?} -> a&a{1,e?} | b&a{1,e?}
regexUnify (ChoiceNode a b) (RepetitionNode l s Nothing n)
    | s <= 1 = regexUnion (regexUnify a n) (regexUnify b n)
regexUnify (RepetitionNode l s Nothing n) (ChoiceNode a b)
    | s <= 1 = regexUnion (regexUnify a n) (regexUnify b n)
--- ### Choice & Group -> empty set

--- ### Repeat & Repeat 
--- #### (overlapping, but not strict subset) -> repeat with (either lazy) (max start) (min end) (subset of node1 and node2)
regexUnify (RepetitionNode l1 s1 e1 n1) (RepetitionNode l2 s2 e2 n2)
    | n1 `isSubNode` n2 = wrapNodeInCaptureGroup [RepetitionNode (l1 || l2) (max s1 s2) (maybeMin e1 e2) n1]
    | n2 `isSubNode` n1 = wrapNodeInCaptureGroup [RepetitionNode (l1 || l2) (max s1 s2) (maybeMin e1 e2) n2]
--- #### otherwise -> empty set
--- ### Repeat & Group -> empty set

--- ### Group & Group -> empty set

--- ### Any with Complement not (direct) subgroup
--- ### Note 1: Inner set = C which becomes complemented (~C)
--- ### Note 2: c in C
--- ### Literal & Complement 
--- #### Epsilon & Complement
--- #### Epsilon in C -> empty set
--- #### Epsilon not in C -> Epsilon
regexUnify (ComplementNode c) ln@(LiteralNode (Left False))
    | ln `isSubNode` c = emptySet
    | otherwise = wrapNodeInCaptureGroup [epsilonNode]
regexUnify ln@(LiteralNode (Left False)) (ComplementNode c)
    | ln `isSubNode` c = emptySet
    | otherwise = wrapNodeInCaptureGroup [epsilonNode]
--- #### AnyChar & Complement 
--- #### AnyChar in C -> empty set
--- #### AnyChar not in C -> AnyChar
regexUnify (ComplementNode c) ln@(LiteralNode (Left True))
    | ln `isSubNode` c = emptySet
    | otherwise = wrapNodeInCaptureGroup [anyCharNode]
regexUnify ln@(LiteralNode (Left True)) (ComplementNode c)
    | ln `isSubNode` c = emptySet
    | otherwise = wrapNodeInCaptureGroup [anyCharNode]
--- #### LitChar & Complement 
--- #### LitChar in C -> empty set
--- #### LitChar not in C -> LitChar
regexUnify (ComplementNode c) ln@(LiteralNode (Right l))
    | ln `isSubNode` c = emptySet
    | otherwise = wrapNodeInCaptureGroup [ln]
regexUnify ln@(LiteralNode (Right l)) (ComplementNode c)
    | ln `isSubNode` c = emptySet
    | otherwise = wrapNodeInCaptureGroup [ln]
--- ### Choice & Complement 
--- #### both a and b in C -> empty set
--- #### a in C -> b
--- #### b in C -> a
--- #### both a and b not in C -> a|b
regexUnify (ComplementNode c) ch@(ChoiceNode a b)
    | a `isSubNode` c && b `isSubNode` c = emptySet
    | a `isSubNode` c = wrapNodeInCaptureGroup [b]
    | b `isSubNode` c = wrapNodeInCaptureGroup [a]
    | otherwise = wrapNodeInCaptureGroup [ch]
regexUnify ch@(ChoiceNode a b) (ComplementNode c)
    | a `isSubNode` c && b `isSubNode` c = emptySet
    | a `isSubNode` c = wrapNodeInCaptureGroup [b]
    | b `isSubNode` c = wrapNodeInCaptureGroup [a]
    | otherwise = wrapNodeInCaptureGroup [ch]
--- ### Repeat & Complement
--- #### c == epsilon -> repeat with at least once
--- #### r in C -> empty set
--- #### c in R -> R\c
--- #### c not in R and r not in C -> r
regexUnify (ComplementNode c) rn@(RepetitionNode l s me r)
    | c == epsilonNode = wrapNodeInCaptureGroup [RepetitionNode l (max 1 s) me r]
    | r `isSubNode` c = emptySet
    | c `isSubNode` rn = regexUnion epsilonNode (RepetitionNode l (max 2 s) me r)
    | otherwise = wrapNodeInCaptureGroup [rn]
regexUnify rn@(RepetitionNode l s me r) (ComplementNode c)
    | c == epsilonNode = wrapNodeInCaptureGroup [RepetitionNode l (max 1 s) me r]
    | r `isSubNode` c = emptySet
    | c `isSubNode` rn = regexUnion epsilonNode (RepetitionNode l (max 2 s) me r)
    | otherwise = wrapNodeInCaptureGroup [rn]
--- ### Group & Complement 
--- #### g in C -> empty set
--- #### g not in C -> g
regexUnify (ComplementNode c) cgs@(CaptureGroupSequence _ g)
    | cgs `isSubNode` c = emptySet
    | otherwise = cgs
regexUnify cgs@(CaptureGroupSequence _ g) (ComplementNode c)
    | cgs `isSubNode` c = emptySet
    | otherwise = cgs

--- ### DEFAULT: empty set
regexUnify x y = emptySet





--- ### NOT IMPLEMENTED SECTION

--- #### Regex Node Extract: Returns the n-th matching of some sub-node in another node
---                          If no match found, return empty set 
---                          (one of the places where an empty set can be introduced in the program)    


--- #### Regex Node Replace: Replace the n-th occurrence of a pattern node with a replacement node in another node
---                          Leaves the original node along if no matching pattern found 
---                          Can be chained for the replaceAll variant

