{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/release-22.05;
    utils.url = github:numtide/flake-utils;
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
      let
        config = { };

        overlay = pkgsNew: pkgsOld: {
          carboncloud =
            pkgsNew.haskell.lib.justStaticExecutables
              pkgsNew.haskellPackages.carboncloud;

          haskellPackages = pkgsOld.haskellPackages.override (old: {
            overrides = pkgsNew.haskell.lib.packageSourceOverrides {
              carboncloud = ./.;
            };
          });
        };

        pkgs =
          import nixpkgs { inherit config system; overlays = [ overlay ]; };

      in
      rec {
        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [ p.carboncloud ];
          buildInputs = with pkgs.haskellPackages; [
            cabal-install
            cabal-fmt
            haskell-language-server
            hlint
            fourmolu
            pkgs.nixpkgs-fmt
          ];
          withHoogle = true;
          doBenchmark = false;
        };
      }
    );
}
