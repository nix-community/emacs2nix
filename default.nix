{ mkDerivation, aeson, async, base, bytestring, containers
, network-uri, stdenv, text
}:
mkDerivation {
  pname = "elpa2nix";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    aeson async base bytestring containers network-uri text
  ];
  homepage = "http://github.com/ttuegel/elpa2nix";
  description = "Automatically generate Nix expressions for Emacs packages";
  license = stdenv.lib.licenses.gpl3;
}
