{ mkDerivation, base, constraints, containers, data-fix, dlist
, fetchgit, free, groups, hedgehog, kan-extensions, mtl
, natural-numbers, stdenv, transformers
}:
mkDerivation {
  pname = "free-algebras";
  version = "0.0.5.1";
  src = fetchgit {
    url = "https://github.com/coot/free-algebras";
    sha256 = "0zc8cx1facyp5psrb1fif36q1blladq44mw11724aqc1bc7j464c";
    rev = "2c8291c1358b4e795f8b65c44706cb2463d3c453";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base constraints containers data-fix dlist free groups
    kan-extensions mtl natural-numbers transformers
  ];
  testHaskellDepends = [
    base constraints containers data-fix dlist free groups hedgehog
    kan-extensions mtl natural-numbers transformers
  ];
  homepage = "https://github.com/coot/free-algebras#readme";
  description = "Free algebras in Haskell";
  license = stdenv.lib.licenses.mpl20;
}
