{ mkDerivation, base, base64-bytestring, blaze-builder, bytestring
, case-insensitive, directory, mtl, network, stdenv, text
, transformers, unordered-containers
}:
mkDerivation {
  pname = "http-common";
  version = "0.8.0.1";
  src = ./http-common;
  buildDepends = [
    base base64-bytestring blaze-builder bytestring case-insensitive
    directory mtl network text transformers unordered-containers
  ];
  homepage = "http://research.operationaldynamics.com/projects/http-streams/";
  description = "Common types for HTTP clients and servers";
  license = stdenv.lib.licenses.bsd3;
}
