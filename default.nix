{ compiler ? "ghc8102"
}:
let compiler-nix-name = compiler;

    sources = import ./nix/sources.nix {};
    iohkNix = import sources.iohk-nix {};
    haskellNix = import sources."haskell.nix" {};
    nixpkgs = iohkNix.nixpkgs;
    haskell-nix = haskellNix.pkgs.haskell-nix;

    # package set
    pkgs = import nixpkgs
              { config = haskellNix.config;
                overlays = [ (_: _: { inherit freeCategoryPackages; }) ];
              };
    lib = pkgs.lib;

    # 'cleanGit' cleans a source directory based on the files known by git
    src = haskell-nix.haskellLib.cleanGit {
      name = "free-category";
      src = ./.;
    };

    # unmodified packages
    projectPackages = lib.attrNames
      (haskell-nix.haskellLib.selectProjectPackages
      (haskell-nix.cabalProject { inherit src compiler-nix-name; }));

    # set GHC options
    freeCategoryPackages = haskell-nix.cabalProject {
        inherit src compiler-nix-name;
        modules =
          [
            { packages =
                lib.genAttrs
                  projectPackages
                  (name: { configureFlags = [ "--ghc-option=-Werror" ]; });
            }
          ];
      };
in pkgs.freeCategoryPackages
