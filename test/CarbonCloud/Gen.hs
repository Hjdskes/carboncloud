module CarbonCloud.Gen (genTree) where

import CarbonCloud (Cost (..), NodeInfo (..), NodeName (..), Tree (..), TypeB (..))
import Hedgehog (Gen, MonadGen, Range)
import Hedgehog.Gen qualified as Gen (alphaNum, choice, element, float, frequency, list, small, string)
import Hedgehog.Range qualified as Range (linear, linearFrac)

genTree :: Gen Tree
genTree =
  Gen.choice
    [ genTreeTypeA
    , genTreeTypeB
    ]
  where
    genTreeTypeA :: Gen Tree
    genTreeTypeA =
      Tree_TypeA
        <$> genNodeInfo
        <*> genString
        <*> genShrinkingList (Range.linear 0 10) genTree

    genTreeTypeB :: Gen Tree
    genTreeTypeB = Tree_TypeB <$> genTypeB

genTypeB :: Gen TypeB
genTypeB =
  TypeB
    <$> genCost
    <*> genNodeName
    <*> genShrinkingList (Range.linear 0 10) genTypeB

genNodeInfo :: Gen NodeInfo
genNodeInfo =
  NodeInfo
    <$> genCost
    <*> genNodeName

genCost :: Gen Cost
genCost = Cost <$> Gen.float (Range.linearFrac 0 10)

-- Generates a node name.
-- We make sure to generate names containing a form of "bepa" frequently,
-- since this is what we want to assert on.
genNodeName :: Gen NodeName
genNodeName =
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

genString :: Gen String
genString = Gen.string (Range.linear 0 10) Gen.alphaNum

-- We want to generate smaller and smaller lists to avoid diverging.
genShrinkingList :: MonadGen m => Range Int -> m a -> m [a]
genShrinkingList range gen = Gen.list range (Gen.small gen)
