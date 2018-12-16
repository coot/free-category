{ compiler ? "ghc844"
, haddock ? true
, test ? true
, benchmarks ? false
, dev ? true
}:
with builtins;
let
  nixpkgs = import ./nix/nixpkgs.nix { inherit compiler; };

  pkgs = nixpkgs.haskell.packages;
  lib = nixpkgs.haskell.lib;

  doHaddock = if haddock
    then lib.doHaddock
    else lib.dontHaddock;
  doTest = if test
    then lib.doCheck
    else lib.dontCheck;
  doBench = if benchmarks
    then lib.doBenchmark
    else nixpkgs.lib.id;
  doDev = if dev
    then drv: lib.appendConfigureFlag drv "--ghc-option -Werror"
    else nixpkgs.lib.id;

  free-category = doDev(doHaddock(doTest(doBench(
    pkgs.${compiler}.callPackage ./pkg.nix
      { inherit nixpkgs; }))));
  examples = doDev(doHaddock(doTest(doBench(
    pkgs.${compiler}.callPackage ./examples/pkg.nix
      { inherit free-category nixpkgs; }))));
in
{ inherit free-category examples; }
