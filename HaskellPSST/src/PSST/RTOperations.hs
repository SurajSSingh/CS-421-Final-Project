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
--- ### Equality or subnode can just use one or the other
regexUnion x y
    | x == y = x
    | x `isSubNode` y = y
    | y `isSubNode` x = x
--- *** No need to check for equality or subnodes below ***
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

--- #### Regex Node Unify: Take the set intersection of two regex trees.
---                        If no unification can occur, then return the empty set 
---                        (one of the places where an empty set can be introduced in the program)              
regexUnify :: RegexNode -> RegexNode -> RegexNode
--- Equality or subnode can just use one or the other
regexUnify x y
    | x == y = y
    | x `isSubNode` y = x
    | y `isSubNode` x = y
--- Everything below is non-equal, non-subsets
--- Literal & Literal (9)
regexUnify (LiteralNode (Left False)) (LiteralNode (Left False)) = epsilonNode
regexUnify (LiteralNode (Left True)) (LiteralNode (Left True)) = anyCharNode
regexUnify (LiteralNode (Left False)) (LiteralNode (Left True)) = optionalNode False anyCharNode
regexUnify (LiteralNode (Left True)) (LiteralNode (Left False)) = optionalNode False anyCharNode
regexUnify (LiteralNode (Left False)) ln@(LiteralNode (Right _)) = optionalNode False ln
regexUnify ln@(LiteralNode (Right _)) (LiteralNode (Left False)) = optionalNode False ln
regexUnify (LiteralNode (Left True)) ln@(LiteralNode (Right _)) = ln
regexUnify ln@(LiteralNode (Right _)) (LiteralNode (Left True)) = ln
regexUnify l1@(LiteralNode (Right x)) l2@(LiteralNode (Right y))
    | x == y = l1
    | otherwise = emptySet

-- regexUnify (LiteralNode (Left False)) (ComplementNode c) = emptySet
-- regexUnify (ComplementNode c) (LiteralNode (Left False)) = emptySet
-- regexUnify (LiteralNode (Left True)) (ComplementNode c) = emptySet
-- regexUnify (ComplementNode c) (LiteralNode (Left True)) = emptySet
-- regexUnify (LiteralNode (Right x)) (ComplementNode c) = emptySet
-- regexUnify (ComplementNode c) (LiteralNode (Right y)) = emptySet
--- Literal & Choice 
regexUnify (LiteralNode (Left False)) (ChoiceNode a b) = emptySet
regexUnify (ChoiceNode a b) (LiteralNode (Left False)) = emptySet
regexUnify (LiteralNode (Left True)) (ChoiceNode a b) = emptySet
regexUnify (ChoiceNode a b) (LiteralNode (Left True)) = emptySet
regexUnify (LiteralNode (Right x)) (ChoiceNode a b) = emptySet
regexUnify (ChoiceNode a b) (LiteralNode (Right y)) = emptySet
--- Literal & Repetition
regexUnify (LiteralNode (Left False)) x = emptySet
regexUnify x (LiteralNode (Left False)) = emptySet
regexUnify (LiteralNode (Left True)) x = emptySet
regexUnify x (LiteralNode (Left True)) = emptySet
regexUnify (LiteralNode (Right x)) c = emptySet
regexUnify x (LiteralNode (Right y)) = emptySet
--- Literal & Group
regexUnify (LiteralNode (Left False)) x = emptySet
regexUnify x (LiteralNode (Left False)) = emptySet
regexUnify (LiteralNode (Left True)) x = emptySet
regexUnify x (LiteralNode (Left True)) = emptySet
regexUnify (LiteralNode (Right x)) c = emptySet
regexUnify x (LiteralNode (Right y)) = emptySet
--- 
regexUnify a b = emptySet
regexUnify a b = emptySet
regexUnify a b = emptySet
regexUnify a b = emptySet
regexUnify a b = emptySet
regexUnify a b = emptySet
regexUnify a b = emptySet
regexUnify a b = emptySet
regexUnify a b = emptySet
--- 
regexUnify (LiteralNode (Left True)) (LiteralNode (Right c)) = LiteralNode (Right c)
regexUnify (LiteralNode (Right c)) (LiteralNode (Left True)) = LiteralNode (Right c)
regexUnify (RepetitionNode l1 s1 e1 n1) (RepetitionNode l2 s2 e2 n2)
    | n1 == n2 = RepetitionNode (l1 || l2) (max s1 s2) (maybeMin e1 e2) n1
--- [] & Complement - all empty since if node were subnode, it would have succeeded earlier 
regexUnify x y = emptySet


--- ### NOT IMPLEMENTED SECTION

--- #### Regex Node Extract: Returns the n-th matching of some sub-node in another node
---                          If no match found, return empty set 
---                          (one of the places where an empty set can be introduced in the program)    


--- #### Regex Node Replace: Replace the n-th occurrence of a pattern node with a replacement node in another node
---                          Leaves the original node along if no matching pattern found 
---                          Can be chained for the replaceAll variant

