{ mkDerivation, aeson, array, base, base16-bytestring, binary
, bytestring, containers, criterion, cryptohash-md5
, cryptohash-sha1, cryptohash-sha256, cryptohash-sha512, data-fix
, deepseq, dependent-sum, deriving-compat, Diff, directory
, exceptions, fetchgit, filepath, free, generic-random, Glob
, hashable, hashing, haskeline, hedgehog, hspec-discover
, http-client, http-client-tls, http-types, interpolate
, lens-family, lens-family-core, lens-family-th, logict, megaparsec
, monadlist, mtl, optparse-applicative, parser-combinators
, pretty-show, prettyprinter, process, ref-tf, regex-tdfa
, regex-tdfa-text, repline, scientific, semigroups, serialise
, split, stdenv, syb, tasty, tasty-hedgehog, tasty-hunit
, tasty-quickcheck, tasty-th, template-haskell, text, these, time
, transformers, unix, unordered-containers, vector, xml
}:
mkDerivation {
  pname = "hnix";
  version = "0.5.2";
  src = fetchgit {
    url = "https://github.com/haskell-nix/hnix.git";
    sha256 = "037kxj9wirynmavlp7d0k19a5xrhj117hlh3yia12xj6v828b99z";
    rev = "617d0867ab96c8f97b02c4524bd948d9f114005e";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array base base16-bytestring binary bytestring containers
    cryptohash-md5 cryptohash-sha1 cryptohash-sha256 cryptohash-sha512
    data-fix deepseq dependent-sum deriving-compat directory exceptions
    filepath free hashable hashing haskeline http-client
    http-client-tls http-types interpolate lens-family lens-family-core
    lens-family-th logict megaparsec monadlist mtl optparse-applicative
    parser-combinators pretty-show prettyprinter process ref-tf
    regex-tdfa regex-tdfa-text scientific semigroups serialise split
    syb template-haskell text these time transformers unix
    unordered-containers vector xml
  ];
  executableHaskellDepends = [
    aeson base base16-bytestring bytestring containers cryptohash-md5
    cryptohash-sha1 cryptohash-sha256 cryptohash-sha512 data-fix
    deepseq exceptions filepath hashing haskeline mtl
    optparse-applicative pretty-show prettyprinter repline serialise
    template-haskell text time transformers unordered-containers
  ];
  testHaskellDepends = [
    base base16-bytestring bytestring containers cryptohash-md5
    cryptohash-sha1 cryptohash-sha256 cryptohash-sha512 data-fix
    deepseq dependent-sum Diff directory exceptions filepath
    generic-random Glob hashing hedgehog interpolate megaparsec mtl
    optparse-applicative pretty-show prettyprinter process serialise
    split tasty tasty-hedgehog tasty-hunit tasty-quickcheck tasty-th
    template-haskell text time transformers unix unordered-containers
  ];
  testToolDepends = [ hspec-discover ];
  benchmarkHaskellDepends = [
    base base16-bytestring bytestring containers criterion
    cryptohash-md5 cryptohash-sha1 cryptohash-sha256 cryptohash-sha512
    data-fix deepseq exceptions filepath hashing mtl
    optparse-applicative serialise template-haskell text time
    transformers unordered-containers
  ];
  homepage = "https://github.com/haskell-nix/hnix#readme";
  description = "Haskell implementation of the Nix language";
  license = stdenv.lib.licenses.bsd3;
}
