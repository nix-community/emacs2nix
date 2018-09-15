let
  nixpkgs = import ./nixpkgs.nix;
  emacs2nix = import ./default.nix;
in

with nixpkgs;

stdenv.mkDerivation {
  name = "interactive-${emacs2nix.name}-environment";
  nativeBuildInputs = [
    emacs nix nix-prefetch-scripts
    bazaar cvs curl fossil git mercurial subversion
    emacs2nix
  ];
  shellHook = ''
    export SSL_CERT_FILE="${cacert}/etc/ssl/certs/ca-bundle.crt"
  '';
}
