{ mkDerivation, adjunctions, array, base, comonad, containers
, contravariant, distributive, free, mtl, semigroupoids, stdenv
, tagged, transformers
}:
mkDerivation {
  pname = "kan-extensions";
  version = "5.0.1";
  sha256 = "1qm0kf4krmyjbjynn96ab0h3q117vwcia5nin7n2b8b4f3jrzph1";
  libraryHaskellDepends = [
    adjunctions array base comonad containers contravariant
    distributive free mtl semigroupoids tagged transformers
  ];
  homepage = "http://github.com/ekmett/kan-extensions/";
  description = "Kan extensions, Kan lifts, various forms of the Yoneda lemma, and (co)density (co)monads";
  license = stdenv.lib.licenses.bsd3;
}
