{ mkDerivation, base, ghc-prim, stdenv, transformers }:
mkDerivation {
  pname = "transformers-compat";
  version = "0.5.1.4";
  sha256 = "17yam0199fh9ndsn9n69jx9nvbsmymzzwbi23dck3dk4q57fz0fq";
  libraryHaskellDepends = [ base ghc-prim transformers ];
  homepage = "http://github.com/ekmett/transformers-compat/";
  description = "A small compatibility shim exposing the new types from transformers 0.3 and 0.4 to older Haskell platforms.";
  license = stdenv.lib.licenses.bsd3;
}
