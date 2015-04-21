with (import <nixpkgs> {}).pkgs;
with haskellngPackages.override {
  overrides = self: super: rec { };
};
let
in
  (callPackage ./. {}).env
