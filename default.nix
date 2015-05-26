{ mkDerivation, aeson, aeson-pretty, async, base, bytestring
, containers, filepath, http-streams, io-streams
, optparse-applicative, process, stdenv, temporary, text
}:
mkDerivation {
  pname = "elpa2nix";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson aeson-pretty async base bytestring containers filepath
    http-streams io-streams optparse-applicative process temporary text
  ];
  homepage = "http://github.com/ttuegel/elpa2nix";
  description = "Automatically generate Nix expressions for Emacs packages";
  license = stdenv.lib.licenses.gpl3;
}
