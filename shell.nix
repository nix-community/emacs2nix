{ nixpkgs ? import ./nixpkgs {}, profiling ? false }:

with nixpkgs;

let
  inherit (pkgs.haskell) lib;
  haskellPackages = pkgs.haskell.packages.ghc801.override {
    overrides = self: super: {

      mkDerivation = args: super.mkDerivation (args // {
        enableLibraryProfiling = profiling;
      });

      hnix = self.callPackage ./hnix {};
    };
  };
  filterSource = drv: pred:
    lib.overrideCabal drv
    (args: args // { src = builtins.filterSource pred args.src; });
  omitDirs =
    let
      omitted = [ ".git" "dist" "nixpkgs" "hnix" ];
    in drv:
    filterSource drv
    (path: type:
      type != "directory" || !(stdenv.lib.elem (baseNameOf path) omitted));
  addCertPath = drv:
    lib.overrideCabal drv
    (args: args // {
      shellHook = ''
        export SSL_CERT_FILE="${cacert}/etc/ssl/certs/ca-bundle.crt"
      '';
    });
  addBuildTools = drv:
    lib.addBuildTools drv
    [
      emacs cabal-install
      nix nix-prefetch-scripts
      bazaar cvs curl darcs fossil git mercurial subversion
    ];
in
addBuildTools (omitDirs (addCertPath (haskellPackages.callPackage ./. {})))
