# Carboncloud interview task

This is my solution to the Carboncloud interview task.

The solution to the first problem is available under `src/CarbonCloud.hs`. The solution to the second problem is available under `test/CarbonCloud/Gen.hs` and `test/CarbonCloud/BepaSpec.hs`.

There is a solution using [`refined`](https://hackage.haskell.org/package/refined) under `src/CarbonCloud/Refined.hs` with tests under `test/CarbonCloud/RefinedSpec.hs`.

## Building & running the code

A Nix development shell is available through `nix develop`. Compiling and testing the code is done through with `cabal build` and `cabal test` respectively.

The code is documented with Haddock, which can be rendered using `cabal haddock --enable-documentation`.
