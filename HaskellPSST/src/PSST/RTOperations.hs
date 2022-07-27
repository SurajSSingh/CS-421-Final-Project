module PSST.RTOperations where

import PSST.Core ( RegexNode (..), epsilonNode, emptySet, isSubNode, wrapNodeInCaptureGroup, everyThingNode )


--- #### Regex Tree Singleton: Does a given regex tree contain only a single valid string 
---      These are:
---       * Any Literal, except AnyCharLiteral, including Epsilon (i.e. {""})
---       * A double complement that has a singleton
---           * Single complement can never be singleton since it's inner value would need to have nearly infinite values 
---       * A sequence with one value that is itself a singleton, example: {"abc"}
---       * A binary choice where both branches are the same and the branches are singleton
---       * Repetition where the start and end match and the tree is a singleton itself
---       * A capture group that has a singleton
---     All else are not singletons (either empty or multiple)
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

--- #### Regex Node Union: Take the set union of two regex trees.
---                        At minimum, a union can occur by creating Choice on both trees
regexUnion :: RegexNode -> RegexNode -> RegexNode
--- Equality or subnode can just use one or the other
regexUnion x y
    | x == y = x
    | x `isSubNode` y = y
    | y `isSubNode` x = x

--- Complement
regexUnion x (ComplementNode c) | x == c = wrapNodeInCaptureGroup [everyThingNode]
regexUnion (ComplementNode c) x | x == c = wrapNodeInCaptureGroup [everyThingNode]
--- Literals
regexUnion (LiteralNode (Left False)) y = wrapNodeInCaptureGroup [RepetitionNode False 0 (Just 1) y]
regexUnion x (LiteralNode (Left False)) = wrapNodeInCaptureGroup  [RepetitionNode False 0 (Just 1) x]
regexUnion (LiteralNode (Left True)) (LiteralNode (Right c)) = wrapNodeInCaptureGroup [LiteralNode (Left True)]
regexUnion (LiteralNode (Right c)) (LiteralNode (Left True)) = wrapNodeInCaptureGroup [LiteralNode (Left True)]

--- Literal/ChoiceNode
regexUnion x (ChoiceNode a b)
    | x `isSubNode` a || x `isSubNode` b = ChoiceNode a b
    | a `isSubNode` x && b `isSubNode` x = x
    | a `isSubNode` x = ChoiceNode x b
    | b `isSubNode` x = ChoiceNode a x
--- Literal/RepetitionNode
regexUnion x r@(RepetitionNode l s Nothing n)
    | x `isSubNode` n && s > 0 = r
--- Literal/CaptureGroupSequence


--- ComplementNode/ComplementNode
--- ComplementNode/ChoiceNode
--- ComplementNode/RepetitionNode
--- ComplementNode/CaptureGroupSequence

--- ChoiceNode/ChoiceNode
--- ChoiceNode/RepetitionNode
--- ChoiceNode/CaptureGroupSequence

--- Repetition/CaptureGroupSequence
--- Repetition/Repetition
regexUnion (RepetitionNode l1 s1 e1 n1) (RepetitionNode l2 s2 e2 n2)
    | n1 == n2 =wrapNodeInCaptureGroup [RepetitionNode (l1 || l2) (min s1 s2) (maybeMax e1 e2) n1]
regexUnion (CaptureGroupSequence n1 s1) (CaptureGroupSequence n2 s2)
    | s1 == s2 = wrapNodeInCaptureGroup [CaptureGroupSequence n1 s1]
regexUnion x y = wrapNodeInCaptureGroup [ChoiceNode x y]

--- #### Regex Node Unify: Take the set intersection of two regex trees.
---                        If no unification can occur, then return the empty set 
---                        (one of the places where an empty set can be introduced in the program)              
regexUnify :: RegexNode -> RegexNode -> RegexNode
--- Equality or subnode can just use one or the other
regexUnify x y
    | x == y = y
    | x `isSubNode` y = x
    | y `isSubNode` x = y
regexUnify x (ComplementNode c) | x == c = emptySet
regexUnify (ComplementNode c) x | x == c = emptySet
--- 
regexUnify (LiteralNode (Left True)) (LiteralNode (Right c)) = LiteralNode (Right c)
regexUnify (LiteralNode (Right c)) (LiteralNode (Left True)) = LiteralNode (Right c)
regexUnify (RepetitionNode l1 s1 e1 n1) (RepetitionNode l2 s2 e2 n2)
    | n1 == n2 = RepetitionNode (l1 || l2) (max s1 s2) (maybeMin e1 e2) n1
regexUnify x y = emptySet


--- ### NOT IMPLEMENTED SECTION

--- #### Regex Node Extract: Returns the n-th matching of some sub-node in another node
---                          If no match found, return empty set 
---                          (one of the places where an empty set can be introduced in the program)    


--- #### Regex Node Replace: Replace the n-th occurrence of a pattern node with a replacement node in another node
---                          Leaves the original node along if no matching pattern found 
---                          Can be chained for the replaceAll variant

