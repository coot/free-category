{ mkDerivation
, nixpkgs
, stdenv
, base
, free-algebras
}:
let
  lib = nixpkgs.lib;
  srcFilter = src: path: type:
    let relPath = lib.removePrefix (toString src + "/") (toString path);
    in 
       lib.hasPrefix "src" relPath
    || lib.hasPrefix "test" relPath
    || lib.any
        (a: a == relPath)
        [ "Setup.hs" "cabal.project" "ChangeLog.md" "free-category.cabal" "LICENSE"];
in
mkDerivation {
  pname = "free-category";
  version = "0.0.1.0";
  src = lib.cleanSourceWith { filter = srcFilter ./.; src = ./.; };
  libraryHaskellDepends = [
    base
    free-algebras
  ];
  libraryToolDepends = [ ];
  testHaskellDepends = [
    base
  ];
  homepage = "https://github.com/coot/free-algebras#readme";
  license = stdenv.lib.licenses.mpl20;
  enableSeparateDocOutput = false;
}
