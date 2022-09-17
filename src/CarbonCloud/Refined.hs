-- |
--Module      : Refined
--Description : CarbonCloud programming challenge with refinement types
--
--Same as "CarbonCloud", but this time using refinement types.
module CarbonCloud.Refined where

import CarbonCloud (NodeInfo (..), NodeName (..), Tree (..), TypeB (..))
import CarbonCloud qualified as Original (getCommonNodeNamesExceptBepa)
import Control.Applicative ((<|>))
import Data.List (isInfixOf)
import Data.Set (Set)
import Data.Set qualified as Set (insert, intersection, toList)
import Data.Text qualified as Text
import Data.Typeable (typeRep)
import Refined (Predicate (..), RefineException, Refined, refine, throwRefineOtherException)
import Refined.Unsafe (reallyUnsafeRefine)

-- | Predicate ensuring that values do not contain strings containing the
-- substring \"Bepa\" (case sensitive) .
--
--   >>> isRight (refine @(NoBepa NodeName) @NodeName (NodeName "Foo"))
--   True
--
--   >>> isLeft (refine @(NoBepa NodeName) @NodeName (NodeName "Bepa"))
--   True
data NoBepa

instance Predicate NoBepa a => Predicate NoBepa [a] where
  validate p = foldr (\a e -> validate p a <|> e) Nothing

instance Predicate NoBepa NodeName where
  validate p (NodeName x) =
    if "Bepa" `isInfixOf` x
      then throwRefineOtherException (typeRep p) ("NodeName '" <> Text.pack x <> "' contains Bepa.")
      else Nothing

instance Predicate NoBepa NodeInfo where
  validate p (NodeInfo _ x) = validate p x

instance Predicate NoBepa TypeB where
  validate p (TypeB _ nodeName subTree) = validate p nodeName >> validate p subTree

instance Predicate NoBepa Tree where
  validate p (Tree_TypeA nodeInfo _ subTree) = validate p nodeInfo >> validate p subTree
  validate p (Tree_TypeB typeB) = validate p typeB

-- | Get all unique 'NodeName's from the given 'Tree'.
getNodeNames :: Refined NoBepa Tree -> Set (Refined NoBepa NodeName)
getNodeNames = foldMap go
  where
    go :: Tree -> Set (Refined NoBepa NodeName)
    go (Tree_TypeA nodeInfo _ subTree) = unsafeInsert (nodeInfoName nodeInfo) go subTree
    go (Tree_TypeB typeB) = getNodeNamesTypeB typeB

    -- 'Refined NoBepa Tree' implies that 'NodeName' does not contain "Bepa". The only thing
    -- that can break this invariant is us, here.
    unsafeInsert :: NodeName -> (b -> Set (Refined NoBepa NodeName)) -> [b] -> Set (Refined NoBepa NodeName)
    unsafeInsert name f subTree = Set.insert (reallyUnsafeRefine name) (foldMap f subTree)

    getNodeNamesTypeB :: TypeB -> Set (Refined NoBepa NodeName)
    getNodeNamesTypeB (TypeB _ nodeName subTree) = unsafeInsert nodeName getNodeNamesTypeB subTree

-- | Get all unique 'NodeName's that are in both 'Tree's. The locations do not have to be the same.
getCommonNodeNames :: Refined NoBepa Tree -> Refined NoBepa Tree -> Set (Refined NoBepa NodeName)
getCommonNodeNames a b = Set.intersection (getNodeNames a) (getNodeNames b)

-- | Get all unique 'NodeName's that are in both 'Tree's, that do not contain the substring \"Bepa\" (case sensitive).
getCommonNodeNamesExceptBepa :: Refined NoBepa Tree -> Refined NoBepa Tree -> [Refined NoBepa NodeName]
getCommonNodeNamesExceptBepa a b = Set.toList (getCommonNodeNames a b)

-- | Get all unique 'NodeName's that are in both 'Tree's, that do not contain the substring \"Bepa\" (case sensitive).
--
-- This is an alternative refinement type implementation, simply refining the original solution from 'CarbonCloud.getCommonNodeNamesExceptBepa' from "CarbonCloud".
-- It doesn't require the reimplementation in this module, but it's also less satisfying in that it doesn't rely on
-- the type system to guarantee correctness of the implementation (insofar that 'getCommonNodeNamesExceptBepa' in this module
-- does, since it relies on 'reallyUnsafeRefine').
getCommonNodeNamesExceptBepa' :: Tree -> Tree -> Either RefineException (Refined NoBepa [NodeName])
getCommonNodeNamesExceptBepa' a b = refine (Original.getCommonNodeNamesExceptBepa a b)
