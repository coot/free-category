{ compiler }:

with builtins;
let default = import ./default.nix
      { haddock    = true;
        test       = true;
        benchmarks = true;
        dev        = true;
        inherit compiler;
      };
in
  { free-category = default.free-category;
    examples      = default.examples;
  }
