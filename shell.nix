{ nixpkgs ? import ./nixpkgs {}, profiling ? false }:

with nixpkgs;

let
  inherit (pkgs.haskell) lib;
  haskellPackages = pkgs.haskell.packages.ghc7103.override {
    overrides = self: super: {

      mkDerivation = args: super.mkDerivation (args // {
        enableLibraryProfiling = profiling;
      });

      comonad = lib.doJailbreak (lib.dontCheck super.comonad);
      distributive = lib.dontCheck super.distributive;
      fail = lib.dontHaddock super.fail;
      hnix = self.callPackage ./hnix {};
      parsers = lib.doJailbreak super.parsers;
      reducers = lib.doJailbreak super.reducers;
      semigroupoids = lib.dontCheck super.semigroupoids;
      trifecta = lib.doJailbreak (lib.dontCheck super.trifecta);
      unordered-containers = lib.doJailbreak super.unordered-containers;
    };
  };
  omitDirs = [ ".git" "dist" "nixpkgs" "hnix" ];
  filterCabalSource = pred: drv:
    lib.overrideCabal drv (args: args // { src = builtins.filterSource pred args.src; });
  addCertPath = drv:
    lib.overrideCabal drv (args: args // {
      shellHook = ''
        export SSL_CERT_FILE="${cacert}/etc/ssl/certs/ca-bundle.crt"
      '';
    });
in
lib.addBuildTools
  (filterCabalSource
    (path: type:
        type != "directory" || !(stdenv.lib.elem (baseNameOf path) omitDirs))
    (addCertPath (haskellPackages.callPackage ./. {})))
[
  emacs cabal-install
  nix nix-prefetch-scripts
  bazaar cvs curl darcs fossil git mercurial subversion
]
