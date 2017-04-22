{ mkDerivation, aeson, ansi-wl-pprint, async, attoparsec, base
, bytestring, containers, data-fix, directory, errors, filepath
, hashable, hnix, http-streams, io-streams, optparse-applicative
, stdenv, taggy, temporary, text, text-regex-replace, transformers
, unordered-containers
}:
mkDerivation {
  pname = "emacs2nix";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-wl-pprint async attoparsec base bytestring containers
    data-fix directory errors filepath hashable hnix http-streams
    io-streams taggy temporary text text-regex-replace transformers
    unordered-containers
  ];
  executableHaskellDepends = [
    aeson async base bytestring containers directory errors filepath
    hnix io-streams optparse-applicative temporary text transformers
    unordered-containers
  ];
  description = "Generate Nix expressions for Emacs packages";
  license = stdenv.lib.licenses.gpl3;
}
