with (import <nixpkgs> {});
let
  inherit (pkgs.haskell) lib;
  haskellPackages = pkgs.haskell.packages.ghc801.override {
    overrides = self: super: {
      aeson = self.callPackage ./aeson.nix {};
      comonad = lib.doJailbreak (lib.dontCheck super.comonad);
      distributive = lib.dontCheck super.distributive;
      fail = lib.dontHaddock super.fail;
      hnix = self.callPackage ./hnix {};
      kan-extensions =
        lib.doJailbreak
        (self.callPackage ./kan-extensions.nix {});
      lens = lib.dontCheck (lib.doJailbreak (self.callPackage ./lens.nix {}));
      parsers = lib.doJailbreak super.parsers;
      reducers = lib.doJailbreak super.reducers;
      semigroupoids = lib.dontCheck super.semigroupoids;
      transformers-compat =
        lib.doJailbreak
        (self.callPackage ./transformers-compat.nix {});
      trifecta = lib.doJailbreak (lib.dontCheck super.trifecta);
      unordered-containers =
        lib.doJailbreak super.unordered-containers;
    };
  };
in
lib.addBuildTools
  (haskellPackages.callPackage ./. {})
  [
    nix-prefetch-scripts git subversion cvs mercurial
    bazaar darcs fossil
  ]
