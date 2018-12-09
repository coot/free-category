{ mkDerivation, base, constraints, containers, data-fix, dlist
, free, groups, hedgehog, kan-extensions, mtl, natural-numbers
, stdenv, transformers
}:
mkDerivation {
  pname = "free-algebras";
  version = "0.0.7.0";
  sha256 = "f0c18b857087ec7ec0321f729562174c9a9f02172806e2fba7c8989fdd7631aa";
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
