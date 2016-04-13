{ nixpkgs ? import <nixpkgs> {}, profiling ? false }:

with nixpkgs;

let
  inherit (pkgs.haskell) lib;
  haskellPackages = pkgs.haskell.packages.ghc7103.override {
    overrides = self: super: {

      mkDerivation = args: super.mkDerivation (args // {
        enableLibraryProfiling = profiling;
      });

      comonad = lib.doJailbreak (lib.dontCheck super.comonad);
      distributive = lib.dontCheck super.distributive;
      fail = lib.dontHaddock super.fail;
      hnix = self.callPackage ./hnix {};
      kan-extensions = self.kan-extensions_5_0_1;
      lens = self.lens_4_13_2_1;
      parsers = lib.doJailbreak super.parsers;
      reducers = lib.doJailbreak super.reducers;
      semigroupoids = lib.dontCheck super.semigroupoids;
      trifecta = lib.doJailbreak (lib.dontCheck super.trifecta);
      unordered-containers = lib.doJailbreak super.unordered-containers;
    };
  };
in
lib.addBuildTools
  (haskellPackages.callPackage ./. {})
  [
    emacs cabal-install
    nix nix-prefetch-scripts
    bazaar cvs curl darcs fossil git mercurial subversion
  ]
