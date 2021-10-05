{ pkgs ? import <nixpkgs> {} }:

let
  inherit (pkgs) lib;

  haskellPackages = pkgs.haskellPackages.override (args: {
    overrides = self: super:
      (args.overrides or (self: super: super)) self super // {
        # Tests cannot be run from Nix builder
        hnix = pkgs.haskell.lib.dontCheck (self.callPackage ./hnix.nix {});
        ghc-heap-view = pkgs.haskell.lib.disableLibraryProfiling super.ghc-heap-view;
        ghc-datasize = pkgs.haskell.lib.disableLibraryProfiling super.ghc-datasize;
      };
  });

  blacklistDirs = [ ".git" "dist" "dist-newstyle" ];
  whitelistExts = [ ".cabal" ".el" ".hs" ];
  whitelistNames = [ "COPYING" "LICENSE" ];

  filterSrc =
    let
      overrideSrc = drv: f:
        let inherit (pkgs.haskell.lib) overrideCabal; in
        overrideCabal drv (args: args // { src = f args.src; });
      predicate = path: type:
        let inherit (lib) any elem hasSuffix; in
        let baseName = baseNameOf path; in
        if type == "directory"
          then !(elem baseName blacklistDirs)
          else any (suf: hasSuffix suf baseName) whitelistExts
            || any (name: baseName == name) whitelistNames;
    in
      drv: overrideSrc drv (src: builtins.filterSource predicate src);

  drv = filterSrc (haskellPackages.callPackage ./package.nix {});

in

  drv
