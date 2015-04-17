{ mkDerivation, base, bytestring, deepseq, HUnit, QuickCheck
, stdenv, test-framework, test-framework-hunit
, test-framework-quickcheck2, text, utf8-string
}:
mkDerivation {
  pname = "blaze-builder";
  version = "0.4.0.1";
  sha256 = "1id3w33x9f7q5m3xpggmvzw03bkp94bpfyz81625bldqgf3yqdn1";
  buildDepends = [ base bytestring deepseq text ];
  testDepends = [
    base bytestring HUnit QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2 text utf8-string
  ];
  homepage = "http://github.com/lpsmith/blaze-builder";
  description = "Efficient buffered output";
  license = stdenv.lib.licenses.bsd3;
}
