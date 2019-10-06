{ mkDerivation, base, containers, data-fix, dlist, free, groups
, hedgehog, kan-extensions, mtl, stdenv, transformers
}:
mkDerivation {
  pname = "free-algebras";
  version = "0.0.8.0";
  sha256 = "47eafb41c60ca509de7078f5d283bd88cf7e1ff81ddb49965d67a9e0e9969b43";
  libraryHaskellDepends = [
    base containers data-fix dlist free groups kan-extensions mtl
    transformers
  ];
  testHaskellDepends = [
    base containers data-fix dlist free groups hedgehog kan-extensions
    mtl transformers
  ];
  homepage = "https://github.com/coot/free-algebras#readme";
  description = "Free algebras in Haskell";
  license = stdenv.lib.licenses.mpl20;
}
