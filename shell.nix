{ compiler ? "ghc865"
, haddock ? true
, test ? true
, benchmarks ? false
, dev ? false
}:
with builtins;
let
  default = import ./default.nix {inherit benchmarks compiler dev haddock test;};
  nixpkgs = import ./nix/nixpkgs.nix { inherit compiler; };
in
  {
    free-category = if nixpkgs.lib.inNixShell
      then default.free-category.env
      else default.free-category;
    examples = if nixpkgs.lib.inNixShell
      then default.examples.env
      else default.examples;
  }
