{ mkDerivation, base, constraints, containers, data-fix, dlist
, fetchgit, free, groups, hedgehog, kan-extensions, mtl
, natural-numbers, stdenv, transformers
}:
mkDerivation {
  pname = "free-algebras";
  version = "0.0.5.1";
  src = fetchgit {
    url = "https://github.com/coot/free-algebras";
    sha256 = "119668xlaqz3pcvv76dcd42py1av32xhzvcx5w5379khcj3l97dc";
    rev = "eae42c8d50fa16ccc354d7a62528cd099f831250";
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
