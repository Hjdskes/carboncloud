module Main (
  main,
) where

import CarbonCloud.BepaSpec (bepaSpec)
import Hedgehog.Main (defaultMain)

main :: IO ()
main =
  defaultMain
    [ bepaSpec
    ]
