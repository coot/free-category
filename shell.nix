{ compiler ? "ghc843"
, haddock ? true
, test ? true
, benchmarks ? false
, dev ? false
}:
with builtins;
let
  default = import ./default.nix {inherit benchmarks compiler dev haddock test;};
  spec = fromJSON (readFile ./nixpkgs.json);
  src = fetchTarball {
    url = "https://github.com/${spec.owner}/${spec.repo}/archive/${spec.rev}.tar.gz";
    sha256 = spec.sha256;
  };
  nixpkgs = import src {};
in
  {
    free-category = if nixpkgs.lib.inNixShell
      then default.free-category.env
      else default.free-category;
  }
