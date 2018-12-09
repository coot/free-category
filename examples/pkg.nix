{ nixpkgs
, mkDerivation
, base
, free-category
, free-algebras
, stdenv
}:
mkDerivation {
  pname = "free-category-examples";
  version = "0.0.1.0";
  src = nixpkgs.lib.sourceFilesBySuffices ./.
    [ ".hs" "LICENSE" "ChangeLog.md" "examples.cabal" ];
  libraryHaskellDepends = [
    base
    free-category
    free-algebras
  ];
  libraryToolDepends = [ ];
  license = stdenv.lib.licenses.mpl20;
  enableSeparateDocOutput = false;
}
