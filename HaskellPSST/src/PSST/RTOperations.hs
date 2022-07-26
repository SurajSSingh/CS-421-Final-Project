module PSST.RTOperations where

import PSST.Core ( RegexNode (..), epsilonNode, emptySetNode, isSubNode, wrapNodeInCaptureGroup )


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


--- #### Regex Node Union: Take the set union of two regex trees.
---                        At minimum, a union can occur by creating Choice on both trees
regexUnion :: RegexNode -> RegexNode -> RegexNode
regexUnion x y 
    | x == y = x
    | x `isSubNode` y = y
    | y `isSubNode` x = x
regexUnion x y = wrapNodeInCaptureGroup [ChoiceNode x y]

--- #### Regex Node Unify: Take the set intersection of two regex trees.
---                        If no unification can occur, then return the empty set 
---                        (one of the places where an empty set can be introduced in the program)              
regexUnify :: RegexNode -> RegexNode -> RegexNode
regexUnify x y 
    | x == y = y
    | x `isSubNode` y = x
    | y `isSubNode` x = y
regexUnify x y = emptySetNode


--- ### NOT IMPLEMENTED SECTION

--- #### Regex Node Extract: Returns the n-th matching of some sub-node in another node
---                          If no match found, return empty set 
---                          (one of the places where an empty set can be introduced in the program)    


--- #### Regex Node Replace: Replace the n-th occurrence of a pattern node with a replacement node in another node
---                          Leaves the original node along if no matching pattern found 
---                          Can be chained for the replaceAll variant

