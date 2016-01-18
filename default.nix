{ mkDerivation, aeson, aeson-pretty, async, attoparsec, base
, bytestring, containers, directory, errors, filepath, hashable
, hnix, io-streams, optparse-applicative, stdenv, temporary, text
, text-regex-replace, transformers, unordered-containers
, wl-pprint-text
}:
mkDerivation {
  pname = "emacs2nix";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async attoparsec base bytestring containers directory errors
    filepath hashable hnix io-streams temporary text text-regex-replace
    transformers unordered-containers wl-pprint-text
  ];
  executableHaskellDepends = [
    aeson aeson-pretty async base bytestring containers directory
    errors filepath io-streams optparse-applicative temporary text
    transformers unordered-containers wl-pprint-text
  ];
  description = "Automatically generate Nix expressions for Emacs packages";
  license = stdenv.lib.licenses.gpl3;
}
