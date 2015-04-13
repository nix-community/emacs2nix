{ mkDerivation, aeson, async, base, network-uri, stdenv }:
mkDerivation {
  pname = "elpa2nix";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ aeson async base network-uri ];
  homepage = "http://github.com/ttuegel/elpa2nix";
  description = "Automatically generate Nix expressions for Emacs packages";
  license = stdenv.lib.licenses.gpl3;
}
