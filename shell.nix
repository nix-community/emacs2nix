with (import <nixpkgs> {}).pkgs;
with haskellngPackages;
(callPackage ./. {}).env
