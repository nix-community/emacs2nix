{ mkDerivation, aeson, aeson-pretty, attoparsec, base, bytestring
, containers, directory, errors, filepath, io-streams
, optparse-applicative, stdenv, temporary, text, transformers
, unordered-containers
}:
mkDerivation {
  pname = "melpa2nix";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson aeson-pretty attoparsec base bytestring containers directory
    errors filepath io-streams optparse-applicative temporary text
    transformers unordered-containers
  ];
  description = "Automatically generate Nix expressions for Emacs packages";
  license = stdenv.lib.licenses.gpl3;
}
