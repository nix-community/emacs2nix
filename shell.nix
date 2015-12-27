with (import <nixpkgs> {}).pkgs;
haskell.lib.addBuildTools
  (haskellPackages.callPackage ./. {})
  [ cabal-install nix-prefetch-scripts git subversion cvs mercurial bazaar darcs fossil ]
