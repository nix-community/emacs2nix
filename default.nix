{ mkDerivation, aeson, ansi-wl-pprint, async, attoparsec, base
, bytestring, containers, data-fix, directory, errors, filepath
, hashable, hnix, io-streams, optparse-applicative, stdenv
, temporary, text, text-regex-replace, transformers
, unordered-containers, cacert
}:
mkDerivation {
  pname = "emacs2nix";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-wl-pprint async attoparsec base bytestring containers
    data-fix directory errors filepath hashable hnix io-streams
    temporary text text-regex-replace transformers unordered-containers
  ];
  executableHaskellDepends = [
    aeson async base bytestring containers directory errors filepath
    hnix io-streams optparse-applicative temporary text transformers
    unordered-containers
  ];
  description = "Automatically generate Nix expressions for Emacs packages";
  license = stdenv.lib.licenses.gpl3;
}
