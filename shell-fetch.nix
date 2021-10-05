{ pkgs ? import <nixpkgs> {} }:

let
  emacs2nix = import ./default.nix { inherit pkgs; };
in

with pkgs;

stdenv.mkDerivation {
  name = "interactive-${emacs2nix.name}-environment";
  nativeBuildInputs = [
    emacs nix nix-prefetch-scripts
    breezy cvs curl fossil git mercurial subversion
    emacs2nix
  ];
  shellHook = ''
    export SSL_CERT_FILE="${cacert}/etc/ssl/certs/ca-bundle.crt"
  '';
}
