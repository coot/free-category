{ mkDerivation, base, constraints, containers, data-fix, dlist
, free, groups, hedgehog, kan-extensions, mtl, natural-numbers
, stdenv, transformers
}:
mkDerivation {
  pname = "free-algebras";
  version = "0.0.6.0";
  sha256 = "5a1d919b76fbe8697fae4c8d79ade62f9616ae44a1fa330aff415f352857628c";
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
