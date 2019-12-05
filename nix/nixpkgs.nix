{ compiler ? "ghc865" }:
with builtins;
let
  rev = if   compiler == "ghc802"
          || compiler == "ghc822"
          || compiler == "ghc844"
    then "722fcbbb80b2142583e9266efe77992f8e32ac4c"
    else "a2f9bcd1328de9c043d7425e45d06fc05e0b7929";
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
            ghc881 = super.haskell.packages.ghc881.override {
              overrides = self: super: overrides self super // {
                cabal-doctest = super.callPackage ./cabal-doctest-1.0.7.nix {};
                haskell-src-exts = super.callPackage ./haskell-src-exts-1.21.1.nix {};
                hedgehog = super.callPackage ./hedgehog-1.0.1.nix {};
              };
            };
            ghc865 = super.haskell.packages.ghc865.override { inherit overrides; };
            ghc844 = super.haskell.packages.ghc844.override { inherit overrides; };
            ghc822 = super.haskell.packages.ghc822.override { inherit overrides; };
            ghc802 = super.haskell.packages.ghc802.override {
              overrides = self: super: overrides self super // {
                ansi-terminal = super.callPackage ./ansi-terminal-0.6.3.1.nix {};
                async = super.callPackage ./async-2.1.1.1.nix {};
                lifted-async = super.callPackage ./lifted-async-0.9.3.3.nix {};
                exceptions = super.callPackage ./exceptions-0.8.3.nix {};
                stm = super.callPackage ./stm-2.4.5.1.nix {};
                concurrent-output = super.callPackage ./concurrent-output-1.9.2.nix {};
              };
            };
          };
        };
      };
    };
  nixpkgs = import (fetchTarball { inherit url; }) { inherit config; };
in nixpkgs
