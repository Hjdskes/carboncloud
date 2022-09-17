module Main (
  main,
) where

import CarbonCloud.BepaSpec (bepaSpec)
import CarbonCloud.RefinedSpec (refinedSpec)
import Hedgehog.Main (defaultMain)

main :: IO ()
main =
  defaultMain
    [ bepaSpec
    , refinedSpec
    ]
