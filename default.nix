{ compiler ? "ghc865"
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
  callCabal2nix = nixpkgs.haskell.packages.${compiler}.callCabal2nix;

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
  noInlineRuleShadowing = if compiler == "ghc802"
    then drv: lib.appendConfigureFlag drv "--ghc-option -Wno-inline-rule-shadowing"
    else nixpkgs.lib.id;
  docNoSeprateOutput = drv: lib.overrideCabal drv (drv: { enableSeparateDocOutput = false; });
  srcFilter = src: path: type:
    let relPath = nixpkgs.lib.removePrefix (toString src + "/") (toString path);
    in 
       nixpkgs.lib.hasPrefix "src" relPath
    || nixpkgs.lib.hasPrefix "test" relPath
    || nixpkgs.lib.hasPrefix "bench" relPath
    || nixpkgs.lib.any
        (a: a == relPath)
        [ "Setup.hs" "cabal.project" "ChangeLog.md" "free-category.cabal" "LICENSE"];

  free-category = noInlineRuleShadowing(docNoSeprateOutput(doDev(doHaddock(doTest(doBench(
    lib.overrideCabal (callCabal2nix "free-category" ./.  {})
      (drv: {src = nixpkgs.lib.cleanSourceWith { filter = srcFilter drv.src; src = drv.src; };})
  ))))));
  examples = docNoSeprateOutput(doDev(doHaddock(doTest(doBench(
    lib.overrideCabal (callCabal2nix "examples" ./examples { inherit free-category; })
      (drv: {src = nixpkgs.lib.sourceFilesBySuffices drv.src [ ".hs" "LICENSE" "ChangeLog.md" "examples.cabal" ];})
  )))));

in { inherit free-category examples; }
