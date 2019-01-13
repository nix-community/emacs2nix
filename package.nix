{ mkDerivation, aeson, async, attoparsec, base, bytestring
, containers, data-fix, directory, errors, exceptions, extra
, filepath, hashable, hnix, hpack, http-streams, io-streams
, optparse-applicative, prettyprinter, scientific, stdenv, taggy
, template-haskell, temporary, text, time, transformers
, unordered-containers
}:
mkDerivation {
  pname = "emacs2nix";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson async attoparsec base bytestring containers data-fix
    directory errors exceptions extra filepath hashable hnix
    http-streams io-streams prettyprinter scientific taggy
    template-haskell temporary text time transformers
    unordered-containers
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson async attoparsec base bytestring containers data-fix
    directory errors exceptions extra filepath hashable hnix
    http-streams io-streams optparse-applicative prettyprinter
    scientific taggy template-haskell temporary text time transformers
    unordered-containers
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/ttuegel/emacs2nix#readme";
  description = "Generate Nix expressions for Emacs packages";
  license = stdenv.lib.licenses.gpl3;
}
