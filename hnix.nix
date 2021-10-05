{ mkDerivation, aeson, ansi-wl-pprint, array, base
, base16-bytestring, binary, bytestring, compact, containers
, criterion, cryptohash-md5, cryptohash-sha1, cryptohash-sha256
, cryptohash-sha512, data-fix, deepseq, deriving-compat, Diff
, directory, exceptions, filepath, generic-random, ghc-datasize
, Glob, hashable, hashing, haskeline, hedgehog, http-client
, http-client-tls, http-types, interpolate, lens-family
, lens-family-core, lens-family-th, logict, megaparsec, monadlist
, mtl, optparse-applicative, pretty-show, process, regex-tdfa
, regex-tdfa-text, repline, scientific, semigroups, serialise
, split, stdenv, syb, tasty, tasty-hedgehog, tasty-hunit
, tasty-quickcheck, tasty-th, template-haskell, text, these, time
, transformers, unix, unordered-containers, vector, xml
, lib
}:
mkDerivation {
  pname = "hnix";
  version = "0.5.1";
  src = ./hnix;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-wl-pprint array base base16-bytestring binary bytestring
    compact containers cryptohash-md5 cryptohash-sha1 cryptohash-sha256
    cryptohash-sha512 data-fix deepseq deriving-compat directory
    exceptions filepath ghc-datasize hashable hashing haskeline
    http-client http-client-tls http-types interpolate lens-family
    lens-family-core lens-family-th logict megaparsec monadlist mtl
    optparse-applicative pretty-show process regex-tdfa regex-tdfa-text
    scientific semigroups serialise split syb template-haskell text
    these time transformers unix unordered-containers vector xml
  ];
  executableHaskellDepends = [
    aeson ansi-wl-pprint base base16-bytestring bytestring compact
    containers cryptohash-md5 cryptohash-sha1 cryptohash-sha256
    cryptohash-sha512 data-fix deepseq exceptions filepath hashing
    haskeline mtl optparse-applicative pretty-show repline serialise
    template-haskell text time transformers unordered-containers
  ];
  testHaskellDepends = [
    ansi-wl-pprint base base16-bytestring bytestring compact containers
    cryptohash-md5 cryptohash-sha1 cryptohash-sha256 cryptohash-sha512
    data-fix deepseq Diff directory exceptions filepath generic-random
    Glob hashing hedgehog interpolate megaparsec mtl
    optparse-applicative pretty-show process serialise split tasty
    tasty-hedgehog tasty-hunit tasty-quickcheck tasty-th
    template-haskell text time transformers unix unordered-containers
  ];
  benchmarkHaskellDepends = [
    ansi-wl-pprint base base16-bytestring bytestring compact containers
    criterion cryptohash-md5 cryptohash-sha1 cryptohash-sha256
    cryptohash-sha512 data-fix deepseq exceptions filepath hashing mtl
    optparse-applicative serialise template-haskell text time
    transformers unordered-containers
  ];
  homepage = "https://github.com/haskell-nix/hnix#readme";
  description = "Haskell implementation of the Nix language";
  license = lib.licenses.bsd3;
}
