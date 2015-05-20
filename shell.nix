with (import <nixpkgs> {}).pkgs;
with haskellngPackages;
(ttuegel.haskell.enableProfiling (callPackage ./. {})).env
