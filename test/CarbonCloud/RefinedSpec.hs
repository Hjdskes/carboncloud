{-# LANGUAGE TemplateHaskell #-}

module CarbonCloud.RefinedSpec (
  refinedSpec,
) where

import CarbonCloud (NodeName (..))
import CarbonCloud qualified
import CarbonCloud.Gen qualified as Gen (genRefinedTree, genTree)
import CarbonCloud.Refined qualified as Refined
import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Set qualified as Set (fromList, isSubsetOf, member, toList)
import Hedgehog (Property, assert, checkParallel, discover, evalEither, footnoteShow, forAll, property, withShrinks)
import Hedgehog.Gen qualified as Gen (resize)
import Refined (unrefine)

-- Assuming that our refinement types are implemented correctly, we now no longer have
-- to prove the property that 'getCommonNodeNamesExceptBepa' or 'getCommonNodeNamesExceptBepa'' return
-- no node names containing "Bepa".
-- However, we still need to prove that the solution contains _all_ node names in common and that
-- it does not add random node names not found in either tree. We might be able to solve for this
-- with more refinement types, but I did not go there.

containsNoBepa :: NodeName -> Bool
containsNoBepa (NodeName name) = not ("Bepa" `isInfixOf` name)

containsNoBepaCI :: NodeName -> Bool
containsNoBepaCI (NodeName name) = not ("bepa" `isInfixOf` map toLower name)

-- All names that are in common between tree A and tree B are in the result of 'getCommonNodeNamesExceptBepa'.
-- This does not guarantee that the result does not contain random node names not found in either tree.
prop_getCommonNodeNamesExceptBepa_result_contains_all_common_names :: Property
prop_getCommonNodeNamesExceptBepa_result_contains_all_common_names = property $ do
  treeA <- forAll $ Gen.resize 30 Gen.genRefinedTree
  treeB <- forAll $ Gen.resize 30 Gen.genRefinedTree
  let nodesFromTreeA = Refined.getNodeNames treeA
      nodesFromTreeB = Refined.getNodeNames treeB
      nodesFromBothTrees = Set.fromList [name | name <- Set.toList nodesFromTreeA, Set.member name nodesFromTreeB]
  assert . Set.isSubsetOf nodesFromBothTrees . Set.fromList $ Refined.getCommonNodeNamesExceptBepa treeA treeB

-- All names that are in common between tree A and tree B are in the result of 'getCommonNodeNamesExceptBepa''.
-- This does not guarantee that the result does not contain random node names not found in either tree.
prop_getCommonNodeNamesExceptBepa'_result_contains_all_common_names :: Property
prop_getCommonNodeNamesExceptBepa'_result_contains_all_common_names = property $ do
  treeA <- forAll $ Gen.resize 30 Gen.genTree
  treeB <- forAll $ Gen.resize 30 Gen.genTree
  let nodesFromTreeA = CarbonCloud.getNodeNames treeA
      nodesFromTreeB = CarbonCloud.getNodeNames treeB
      nodesFromBothTrees = Set.fromList [name | name <- Set.toList nodesFromTreeA, Set.member name nodesFromTreeB, containsNoBepa name]
  commonNodeNamesExceptBepa <- evalEither $ Refined.getCommonNodeNamesExceptBepa' treeA treeB
  assert . Set.isSubsetOf nodesFromBothTrees $ Set.fromList (unrefine commonNodeNamesExceptBepa)

-- The result of 'getCommonNodeNamesExceptBepa' is a subset of all node names that are in common between
-- tree A and tree B. It cannot be a proper subset because of the empty case. This proves that the solution
-- does not add random names that are not in either tree.
prop_getCommonNodeNamesExceptBepa_result_is_subset_of_common_node_names :: Property
prop_getCommonNodeNamesExceptBepa_result_is_subset_of_common_node_names = property $ do
  treeA <- forAll $ Gen.resize 30 Gen.genRefinedTree
  treeB <- forAll $ Gen.resize 30 Gen.genRefinedTree
  let nodesFromBothTrees = Set.fromList $ Refined.getCommonNodeNamesExceptBepa treeA treeB
  assert $ Set.isSubsetOf nodesFromBothTrees (Refined.getNodeNames treeA)
  assert $ Set.isSubsetOf nodesFromBothTrees (Refined.getNodeNames treeB)

-- The result of 'getCommonNodeNamesExceptBepa'' is a subset of all node names that are in common between
-- tree A and tree B. It cannot be a proper subset because of the empty case. This proves that the solution
-- does not add random names that are not in either tree.
prop_getCommonNodeNamesExceptBepa'_result_is_subset_of_common_node_names :: Property
prop_getCommonNodeNamesExceptBepa'_result_is_subset_of_common_node_names = property $ do
  treeA <- forAll $ Gen.resize 30 Gen.genTree
  treeB <- forAll $ Gen.resize 30 Gen.genTree
  nodesFromBothTrees <- fmap (Set.fromList . unrefine) . evalEither $ Refined.getCommonNodeNamesExceptBepa' treeA treeB
  assert $ Set.isSubsetOf nodesFromBothTrees (CarbonCloud.getNodeNames treeA)
  assert $ Set.isSubsetOf nodesFromBothTrees (CarbonCloud.getNodeNames treeB)

-- The result of 'getCommonNodeNamesExceptBepa' contains no strings containing "bepa" (case insensitive).
-- This property should fail.
prop_getCommonNodeNamesExceptBepa_result_contains_no_bepa_ci :: Property
prop_getCommonNodeNamesExceptBepa_result_contains_no_bepa_ci = withShrinks 0 . property $ do
  treeA <- forAll $ Gen.resize 30 Gen.genRefinedTree
  treeB <- forAll $ Gen.resize 30 Gen.genRefinedTree
  let nodesFromBothTrees = map unrefine $ Refined.getCommonNodeNamesExceptBepa treeA treeB
  footnoteShow nodesFromBothTrees
  assert $ all containsNoBepaCI nodesFromBothTrees

-- The result of 'getCommonNodeNamesExceptBepa'' contains no strings containing "bepa" (case insensitive).
-- This property should fail.
prop_getCommonNodeNamesExceptBepa'_result_contains_no_bepa_ci :: Property
prop_getCommonNodeNamesExceptBepa'_result_contains_no_bepa_ci = withShrinks 0 . property $ do
  treeA <- forAll $ Gen.resize 30 Gen.genTree
  treeB <- forAll $ Gen.resize 30 Gen.genTree
  nodesFromBothTrees <- fmap unrefine . evalEither $ Refined.getCommonNodeNamesExceptBepa' treeA treeB
  footnoteShow nodesFromBothTrees
  assert $ all containsNoBepaCI nodesFromBothTrees

refinedSpec :: IO Bool
refinedSpec = checkParallel $$discover
