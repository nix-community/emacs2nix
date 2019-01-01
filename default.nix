let nixpkgs = import ./nixpkgs.nix; in

let

  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskell haskellPackages lib;


  blacklistDirs = [ ".git" "dist" "dist-newstyle" ];
  whitelistExts = [ ".cabal" ".el" ".hs" ];
  whitelistNames = [ "COPYING" "LICENSE" "package.yaml" ];

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

  drv =
    lib.foldr lib.id
      (haskellPackages.callPackage ./package.nix {})
      [
        haskell.lib.disableLibraryProfiling
        filterSrc
      ];

in

drv
