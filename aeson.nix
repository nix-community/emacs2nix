{ mkDerivation, attoparsec, base, bytestring, containers, deepseq
, dlist, fail, ghc-prim, hashable, HUnit, mtl, QuickCheck
, quickcheck-instances, scientific, semigroups, stdenv, syb
, template-haskell, test-framework, test-framework-hunit
, test-framework-quickcheck2, text, time, transformers
, unordered-containers, vector
}:
mkDerivation {
  pname = "aeson";
  version = "0.11.0.0";
  sha256 = "1mdd4klbad1dx5854agiiixfcc269hnmbam31zmfs91qszaljf5d";
  libraryHaskellDepends = [
    attoparsec base bytestring containers deepseq dlist fail ghc-prim
    hashable mtl scientific semigroups syb template-haskell text time
    transformers unordered-containers vector
  ];
  testHaskellDepends = [
    attoparsec base bytestring containers ghc-prim HUnit QuickCheck
    quickcheck-instances template-haskell test-framework
    test-framework-hunit test-framework-quickcheck2 text time
    unordered-containers vector
  ];
  homepage = "https://github.com/bos/aeson";
  description = "Fast JSON parsing and encoding";
  license = stdenv.lib.licenses.bsd3;
}
