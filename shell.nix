{ nixpkgs ? import ./nixpkgs {}, profiling ? false }:

with nixpkgs;

let
  inherit (pkgs.haskell) lib;
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
(addBuildTools (addCertPath (import ./default.nix { inherit nixpkgs profiling; }))).env
