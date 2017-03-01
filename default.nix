{ nixpkgs ? import ./nixpkgs {}, profiling ? false }:

with nixpkgs;

let
  inherit (pkgs.haskell) lib;
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {

      mkDerivation = args: super.mkDerivation (args // {
        enableLibraryProfiling = profiling;
      });

      hnix = self.callPackage ./hnix/project.nix {};
    };
  };
  filterSource =
    let
      overrideSrc = drv: f:
        lib.overrideCabal drv (args: args // { src = f args.src; });
    in
      drv: pred: overrideSrc drv (src: builtins.filterSource pred src);
  omitDirs =
    let
      blacklistDirs = [ ".git" "dist" "nixpkgs" "hnix" ];
      whitelistExts = [ ".cabal" ".hs" ".el" ];
      whitelistNames = [ "LICENSE" ];
      inherit (stdenv) lib;
      predicate = path: type:
        let baseName = baseNameOf path; in
        if type == "directory"
          then !(lib.elem baseName blacklistDirs)
          else lib.any (suf: lib.hasSuffix suf baseName) whitelistExts
            || lib.any (name: baseName == name) whitelistNames;
    in
      drv: filterSource drv predicate;
in
omitDirs (haskellPackages.callPackage ./emacs2nix.nix {})
