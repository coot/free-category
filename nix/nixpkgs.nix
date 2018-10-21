{}:
with builtins;
let
  rev = "61deecdc34fc609d0f805b434101f3c8ae3b807a";
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  config =
    { packageOverrides = super:
      let self = super.pkgs;
          lib = super.haskell.lib;
          overrides = self: super: {
            free-algebras = super.callPackage ./free-algebras.nix {};
          };
      in {
        haskell = super.haskell // {
          packages = super.haskell.packages // {
            ghc861 = super.haskell.packages.ghc861.override { inherit overrides; };
            ghc843 = super.haskell.packages.ghc843.override { inherit overrides; };
            ghc822 = super.haskell.packages.ghc822.override { inherit overrides; };
            ghc802 = super.haskell.packages.ghc802.override { inherit overrides; };
          };
        };
      };
    };
  nixpkgs = import (fetchTarball { inherit url; }) { inherit config; };
in nixpkgs
