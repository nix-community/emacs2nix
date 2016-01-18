with (import <nixpkgs> {}).pkgs;
let haskellPackagesOrig = haskellPackages; in
let
  haskellPackages = haskellPackagesOrig.override {
    overrides = self: super: {
      hnix = self.callPackage ./hnix {};
    };
  };
in
haskell.lib.addBuildTools
  (haskellPackages.callPackage ./. {})
  [ cabal-install nix-prefetch-scripts git subversion cvs mercurial bazaar darcs fossil ]
