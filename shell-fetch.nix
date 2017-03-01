{ nixpkgs ? import ./nixpkgs {}, profiling ? false }:

# Shell environment used by the fetching scripts

with nixpkgs;

let emacs2nix = import ./default.nix { inherit nixpkgs profiling; }; in

stdenv.mkDerivation {
  name = "interactive-${emacs2nix.name}-environment";
  nativeBuildInputs = [
    emacs nix nix-prefetch-scripts
    bazaar cvs curl darcs fossil git mercurial subversion
    emacs2nix
  ];
  shellHook = ''
    export SSL_CERT_FILE="${cacert}/etc/ssl/certs/ca-bundle.crt"
  '';
}
