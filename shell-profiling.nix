with (import <nixpkgs> {}).pkgs;
let haskellPackages = pkgs.haskellPackages.override {
      overrides = self: super: {
        mkDerivation = args: super.mkDerivation (args // {
          enableLibraryProfiling = true;
        });
      };
    };
in
(haskellPackages.callPackage ./. {}).env
