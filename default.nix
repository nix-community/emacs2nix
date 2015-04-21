{ mkDerivation, aeson, base, stdenv, unordered-containers }:
mkDerivation {
  pname = "melpa2nix";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ aeson base unordered-containers ];
  description = "Automatically generate Nix expressions for Emacs packages";
  license = stdenv.lib.licenses.gpl3;
}
