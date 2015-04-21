with (import <nixpkgs> {}).pkgs;
with haskellngPackages.override {
  overrides = self: super: rec {
    blaze-builder = self.callPackage ./blaze-builder.nix {};
    http-streams = haskell-ng.lib.overrideCabal (self.callPackage ./http-streams.nix {
      http-common = self.callPackage ./http-common.nix {};
    }) (drv: drv // { doCheck = false; });
  };
};
let
in
  (callPackage ./. {}).env
