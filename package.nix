{ mkDerivation, aeson, ansi-wl-pprint, async, attoparsec, base
, bytestring, containers, data-fix, directory, errors, filepath
, hashable, hnix, http-streams, io-streams, optparse-applicative
, scientific, stdenv, taggy, template-haskell, temporary, text
, time, transformers, unordered-containers
}:
mkDerivation {
  pname = "emacs2nix";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson ansi-wl-pprint async attoparsec base bytestring containers
    data-fix directory errors filepath hashable hnix http-streams
    io-streams scientific taggy template-haskell temporary text time
    transformers unordered-containers
  ];
  executableHaskellDepends = [
    aeson ansi-wl-pprint async attoparsec base bytestring containers
    data-fix directory errors filepath hashable hnix http-streams
    io-streams optparse-applicative scientific taggy template-haskell
    temporary text time transformers unordered-containers
  ];
  homepage = "https://github.com/ttuegel/emacs2nix#readme";
  description = "Generate Nix expressions for Emacs packages";
  license = stdenv.lib.licenses.gpl3;
}
