module PSST.RTOperations where

import PSST.Core ( RegexNode (..), epsilonNode, emptySet, isSubNode, wrapNodeInCaptureGroup, everyThingNode, optionalNode, anyCharNode, maybeMax, maybeMin )


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


--- ## Regex Node Union: Take the set union of two regex trees.
---                      At minimum, a union can occur by creating Choice on both trees
regexUnion :: RegexNode -> RegexNode -> RegexNode
--- ### Equality or subnode can just use larger of two
--- #### Includes Special Case: Empty Set -> return node
regexUnion x y
    | x == y = y
    | x `isSubNode` y = y
    | y `isSubNode` x = x
--- *** Every Node below is non-equal, non-subsets, non-empty ***
--- ### Literal | Literal
--- #### AnyChar | Epsilon -> optional any char
regexUnion (LiteralNode (Left False)) (LiteralNode (Left True)) = optionalNode False anyCharNode
regexUnion (LiteralNode (Left True)) (LiteralNode (Left False)) = optionalNode False anyCharNode
--- #### LitChar | Epsilon -> optional literal
regexUnion (LiteralNode (Left False)) ln@(LiteralNode (Right c)) = optionalNode False ln
regexUnion ln@(LiteralNode (Right c)) (LiteralNode (Left False)) = optionalNode False ln
--- #### LitChar | AnyChar -> any char
regexUnion (LiteralNode (Left True)) ln@(LiteralNode (Right c)) = anyCharNode
regexUnion ln@(LiteralNode (Right c)) (LiteralNode (Left True)) = anyCharNode
--- #### LitChar | LitChar -> l1 | l2
--- ### Literal | Choice
--- #### Epsilon | Choice -> optional choice
regexUnion (LiteralNode (Left False)) cn@(ChoiceNode a b) = optionalNode False cn
regexUnion cn@(ChoiceNode a b) (LiteralNode (Left False)) = optionalNode False cn
--- #### AnyChar | Choice -> . | choice)
--- #### LitChar | Choice -> c | choice)
--- ### Literal | Repetition
--- #### Epsilon | Repetition{1,e?} -> repeat*
regexUnion (LiteralNode (Left False)) (RepetitionNode l 1 e n) = RepetitionNode l 0 e n
--- #### Epsilon | Repetition{2+,e?} -> "" | repeat
--- #### AnyChar | Repetition -> . | repeat
--- #### LitChar | Repetition -> c | repeat
--- ### Literal | Group
--- #### Epsilon | Group -> "" | group
--- #### AnyChar | Group -> any | group
--- #### LitChar | Group -> c | group
--- ### Choice | Choice -> c1 | c2
--- ### Choice | Repetition -> c1 | rep
--- ### Choice | Group -> c1 | group
--- ### Repetition | Repetition -> rep1 | rep2
--- ### Repetition | Group -> rep | group
--- ### Group | Group -> g1 | g2

--- ### Any/Complement when any is complemented -> everything
regexUnion x (ComplementNode c) | x == c = everyThingNode
regexUnion (ComplementNode c) x | x == c = everyThingNode
--- ### DEFAULT: a b = ChoiceNode a b
regexUnion x y = ChoiceNode x y

--- ## Regex Node Unify: Take the set intersection of two regex trees.
---                      If no unification can occur, then return the empty set 
---                      (one of the places where an empty set can be introduced in the program)              
regexUnify :: RegexNode -> RegexNode -> RegexNode
--- ### Equality or subnode can just use smaller for two
--- #### Includes Special Case: Empty Set -> return empty set
regexUnify x y
    | x == y = x
    | x `isSubNode` y = x
    | y `isSubNode` x = y
--- *** Every Node below is non-equal, non-subsets, non-empty ***
--- ### Literal & Literal
--- #### Epsilon & AnyChar -> empty set (any char is only non-empty character)
--- #### Epsilon & LitChar -> empty set
--- #### LitChar & AnyChar -> char
regexUnify (LiteralNode (Left True)) ln@(LiteralNode (Right c)) = ln
regexUnify ln@(LiteralNode (Right c)) (LiteralNode (Left True)) = ln
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
    | a == c = a
    | a == d = a
    | b == c = b
    | b == d = b
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
    | n1 == n2 = RepetitionNode (l1 || l2) (max s1 s2) (maybeMin e1 e2) n1
    | n1 `isSubNode` n2 = RepetitionNode (l1 || l2) (max s1 s2) (maybeMin e1 e2) n1
    | n2 `isSubNode` n1 = RepetitionNode (l1 || l2) (max s1 s2) (maybeMin e1 e2) n2
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
    | otherwise = epsilonNode
regexUnify ln@(LiteralNode (Left False)) (ComplementNode c)
    | ln `isSubNode` c = emptySet
    | otherwise = epsilonNode
--- #### AnyChar & Complement 
--- #### AnyChar in C -> empty set
--- #### AnyChar not in C -> AnyChar
regexUnify (ComplementNode c) ln@(LiteralNode (Left True))
    | ln `isSubNode` c = emptySet
    | otherwise = anyCharNode
regexUnify ln@(LiteralNode (Left True)) (ComplementNode c)
    | ln `isSubNode` c = emptySet
    | otherwise = anyCharNode
--- #### LitChar & Complement 
--- #### LitChar in C -> empty set
--- #### LitChar not in C -> LitChar
regexUnify (ComplementNode c) ln@(LiteralNode (Right l))
    | ln `isSubNode` c = emptySet
    | otherwise = ln
regexUnify ln@(LiteralNode (Right l)) (ComplementNode c)
    | ln `isSubNode` c = emptySet
    | otherwise = ln
--- ### Choice & Complement 
--- #### both a and b in C -> empty set
--- #### a in C -> b
--- #### b in C -> a
--- #### both a and b not in C -> a|b
regexUnify (ComplementNode c) ch@(ChoiceNode a b)
    | a `isSubNode` c && b `isSubNode` c = emptySet
    | a `isSubNode` c = b
    | b `isSubNode` c = a
    | otherwise = ch
regexUnify ch@(ChoiceNode a b) (ComplementNode c)
    | a `isSubNode` c && b `isSubNode` c = emptySet
    | a `isSubNode` c = b
    | b `isSubNode` c = a
    | otherwise = ch
--- ### Repeat & Complement
--- #### c == epsilon -> repeat with at least once
--- #### r in C -> empty set
--- #### c in R -> R\c
--- #### c not in R and r not in C -> r
regexUnify (ComplementNode c) rn@(RepetitionNode l s me r)
    | c == epsilonNode = RepetitionNode l (max 1 s) me r
    | r `isSubNode` c = emptySet
    | c `isSubNode` rn = regexUnion epsilonNode (RepetitionNode l (max 2 s) me r)
    | otherwise = rn
regexUnify rn@(RepetitionNode l s me r) (ComplementNode c)
    | c == epsilonNode = RepetitionNode l (max 1 s) me r
    | r `isSubNode` c = emptySet
    | c `isSubNode` rn = regexUnion epsilonNode (RepetitionNode l (max 2 s) me r)
    | otherwise = rn
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

