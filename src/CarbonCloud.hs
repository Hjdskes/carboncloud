-- |
--Module      : CarbonCloud
--Description : CarbonCloud programming challenge
--
--For this exercise, use haskell. There are 2 parts of this exercise.
--You're welcome, but not required, to embellish or add extra requirements/functionality.
--
--    1. Implement the function 'getCommonNodeNamesExceptBepa'.
--
--        - The function should return all node names that exists in both the trees (the name does not need to be in the same place in the tree)
--        - Names that contains the string "\Bepa\" (case sensitive) should never be included in the result
--
--    2. Implement "property-based tests" to the function 'getCommonNodeNamesExceptBepa'.
--
--        - Write generators for all the types described above
--        - Identify as many properties as possible and write tests for them
--        - See if you can find ways to update the types (possibly with new types) to reduce the number of tests needed to ensure the properties
--        - Finally, write a test for the property \"Nodes with the name \"bepa\" (case /insensitive/) should never be present in the results\". This property should fail without changing the implementation.
--
--If you want to use GDP(Ghosts of departed proofs) or Refinement Types, please do. It is not a requirement and not 100% that it is suitable for this task and your solution.
module CarbonCloud where

import Data.List (isInfixOf)
import Data.Set (Set)
import Data.Set qualified as Set (filter, insert, intersection, toList)

newtype NodeName = NodeName String deriving (Eq, Ord, Show)

newtype Cost = Cost Float deriving (Show)

data NodeInfo = NodeInfo
  { cost :: Cost
  , nodeInfoName :: NodeName
  }
  deriving (Show)

data Tree
  = Tree_TypeA NodeInfo String [Tree]
  | Tree_TypeB TypeB
  deriving (Show)

data TypeB = TypeB Cost NodeName [TypeB] deriving (Show)

-- | Get all unique 'NodeName's from the given 'Tree'.
getNodeNames :: Tree -> Set NodeName
getNodeNames = \case
  Tree_TypeA nodeInfo _ subTree -> go (nodeInfoName nodeInfo) getNodeNames subTree
  Tree_TypeB typeB -> getNodeNamesTypeB typeB
  where
    go :: NodeName -> (b -> Set NodeName) -> [b] -> Set NodeName
    go name f subTree = Set.insert name (foldMap f subTree)

    getNodeNamesTypeB :: TypeB -> Set NodeName
    getNodeNamesTypeB (TypeB _ nodeName subTree) = go nodeName getNodeNamesTypeB subTree

-- | Get all unique 'NodeName's that are in both 'Tree's. The locations do not have to be the same.
getCommonNodeNames :: Tree -> Tree -> Set NodeName
getCommonNodeNames a b = Set.intersection (getNodeNames a) (getNodeNames b)

-- | Get all unique 'NodeName's that are in both 'Tree's, that do not contain the substring \"Bepa\" (case sensitive).
getCommonNodeNamesExceptBepa :: Tree -> Tree -> [NodeName]
getCommonNodeNamesExceptBepa a b = Set.toList . Set.filter (not . containsBepa) $ getCommonNodeNames a b
  where
    containsBepa :: NodeName -> Bool
    containsBepa (NodeName name) = "Bepa" `isInfixOf` name
