{-# LANGUAGE TemplateHaskell #-}

module CarbonCloud.BepaSpec (
  bepaSpec,
) where

import CarbonCloud (NodeName (..), getCommonNodeNamesExceptBepa, getNodeNames)
import CarbonCloud.Gen qualified as Gen (genTree)
import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Set qualified as Set (fromList, isSubsetOf, member, toList)
import Hedgehog (Property, assert, checkParallel, discover, footnoteShow, forAll, property, withShrinks)
import Hedgehog.Gen qualified as Gen (resize)

containsNoBepa :: NodeName -> Bool
containsNoBepa (NodeName name) = not ("Bepa" `isInfixOf` name)

containsNoBepaCI :: NodeName -> Bool
containsNoBepaCI (NodeName name) = not ("bepa" `isInfixOf` map toLower name)

-- The result of 'getCommonNodeNamesExceptBepa' contains no node names containing "Bepa".
-- This proves the main property of the solution, but it doesn't prove that the solution
-- contains _all_ node names in common not containing "Bepa", nor that it does not add
-- random node names not found in either tree.
prop_getCommonNodeNamesExceptBepa_result_contains_no_bepa :: Property
prop_getCommonNodeNamesExceptBepa_result_contains_no_bepa = property $ do
  treeA <- forAll $ Gen.resize 30 Gen.genTree
  treeB <- forAll $ Gen.resize 30 Gen.genTree
  assert . all containsNoBepa $ getCommonNodeNamesExceptBepa treeA treeB

-- All names that are in common between tree A and tree B that do not contain the substring "Bepa",
-- are in the result of 'getCommonNodeNamesExceptBepa'. This proves that the solution contains _all_
-- node names in common not containing "Bepa", but it does not guarantee that the result does not
-- contain random node names not containing "Bepa" not found in either tree.
prop_getCommonNodeNamesExceptBepa_result_contains_all_common_names_not_containing_bepa :: Property
prop_getCommonNodeNamesExceptBepa_result_contains_all_common_names_not_containing_bepa = property $ do
  treeA <- forAll $ Gen.resize 30 Gen.genTree
  treeB <- forAll $ Gen.resize 30 Gen.genTree
  let nodesFromTreeA = getNodeNames treeA
      nodesFromTreeB = getNodeNames treeB
      nodesFromBothTrees = Set.fromList [name | name <- Set.toList nodesFromTreeA, Set.member name nodesFromTreeB, containsNoBepa name]
  assert . Set.isSubsetOf nodesFromBothTrees . Set.fromList $ getCommonNodeNamesExceptBepa treeA treeB

-- The result of 'getCommonNodeNamesExceptBepa' is a subset of all node names that are in common between
-- tree A and tree B. It cannot be a proper subset because of the empty case. This proves that the solution
-- does not add random names that are not in either tree.
prop_getCommonNodeNamesExceptBepa_result_is_subset_of_common_node_names :: Property
prop_getCommonNodeNamesExceptBepa_result_is_subset_of_common_node_names = property $ do
  treeA <- forAll $ Gen.resize 30 Gen.genTree
  treeB <- forAll $ Gen.resize 30 Gen.genTree
  let nodesFromBothTrees = Set.fromList $ getCommonNodeNamesExceptBepa treeA treeB
  assert $ Set.isSubsetOf nodesFromBothTrees (getNodeNames treeA)
  assert $ Set.isSubsetOf nodesFromBothTrees (getNodeNames treeB)

-- The result of 'getCommonNodeNamesExceptBepa' contains no strings containing "bepa" (case insensitive).
-- This property should fail.
prop_getCommonNodeNamesExceptBepa_result_contains_no_bepa_ci :: Property
prop_getCommonNodeNamesExceptBepa_result_contains_no_bepa_ci = withShrinks 0 . property $ do
  treeA <- forAll $ Gen.resize 30 Gen.genTree
  treeB <- forAll $ Gen.resize 30 Gen.genTree
  let nodesFromBothTrees = getCommonNodeNamesExceptBepa treeA treeB
  footnoteShow nodesFromBothTrees
  assert $ all containsNoBepaCI nodesFromBothTrees

bepaSpec :: IO Bool
bepaSpec = checkParallel $$discover
