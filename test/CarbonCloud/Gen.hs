module CarbonCloud.Gen (
  genRefinedTree,
  genTree,
) where

import CarbonCloud (Cost (..), NodeInfo (..), NodeName (..), Tree (..), TypeB (..))
import CarbonCloud.Refined (NoBepa)
import Data.Either.Combinators (rightToMaybe)
import Hedgehog (Gen, MonadGen, Range)
import Hedgehog.Gen qualified as Gen (alphaNum, choice, element, float, frequency, list, mapMaybe, small, string)
import Hedgehog.Range qualified as Range (linear, linearFrac)
import Refined (Refined, refine)

genTree :: Gen Tree
genTree = genTree' genNodeNameWithBepa

genRefinedTree :: Gen (Refined NoBepa Tree)
genRefinedTree = Gen.mapMaybe rightToMaybe $ refine <$> genTree' genNodeNameWithoutBepaCS

genTree' :: Gen NodeName -> Gen Tree
genTree' genNodeName =
  Gen.choice
    [ genTreeTypeA
    , genTreeTypeB
    ]
  where
    genTreeTypeA :: Gen Tree
    genTreeTypeA =
      Tree_TypeA
        <$> genNodeInfo genNodeName
        <*> genString
        <*> genShrinkingList (Range.linear 0 10) (genTree' genNodeName)

    genTreeTypeB :: Gen Tree
    genTreeTypeB = Tree_TypeB <$> genTypeB genNodeName

genTypeB :: Gen NodeName -> Gen TypeB
genTypeB genNodeName =
  TypeB
    <$> genCost
    <*> genNodeName
    <*> genShrinkingList (Range.linear 0 10) (genTypeB genNodeName)

genNodeInfo :: Gen NodeName -> Gen NodeInfo
genNodeInfo genNodeName =
  NodeInfo
    <$> genCost
    <*> genNodeName

genCost :: Gen Cost
genCost = Cost <$> Gen.float (Range.linearFrac 0 10)

-- Generates a node name likely to contain a form of "bepa" (case insensitive).
genNodeNameWithBepa :: Gen NodeName
genNodeNameWithBepa =
  NodeName
    <$> Gen.frequency
      [ (3, genBepaString)
      , (6, genString <> genBepaString)
      , (6, genBepaString <> genString)
      , (6, genString <> genBepaString <> genString)
      , (1, genString)
      ]
  where
    genBepaString :: Gen String
    genBepaString = Gen.element ["bepa", "Bepa", "BEPA"]

-- Generates a node name without "Bepa" (case sensitive).
genNodeNameWithoutBepaCS :: Gen NodeName
genNodeNameWithoutBepaCS =
  NodeName
    <$> Gen.frequency
      [ (3, genBepaString)
      , (6, genString <> genBepaString)
      , (6, genBepaString <> genString)
      , (6, genString <> genBepaString <> genString)
      , (1, genString)
      ]
  where
    genBepaString :: Gen String
    genBepaString = Gen.element ["bepa", "BEPA"]

genString :: Gen String
genString = Gen.string (Range.linear 0 10) Gen.alphaNum

-- We want to generate smaller and smaller lists to avoid diverging.
genShrinkingList :: MonadGen m => Range Int -> m a -> m [a]
genShrinkingList range gen = Gen.list range (Gen.small gen)
