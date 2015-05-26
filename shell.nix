with (import <nixpkgs> {}).pkgs;
with haskellPackages;
(callPackage ./. {}).env
