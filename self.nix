let nixpkgs = import ./nixpkgs.nix; in

let inherit (nixpkgs) lib; in

let
  self =
    let
      Derivation = import ./Derivation.nix {
        inherit (nixpkgs)
          lib stdenv
          fetchgit fetchhg fetchurl fetchFromGitHub fetchFromGitLab
          emacs texinfo;
        packageBuild =
          let lock = lib.importJSON ./package-build.lock.json; in
          nixpkgs.fetchFromGitHub {
            owner = "melpa";
            repo = "package-build";
            inherit (lock) rev sha256;
          };
      };
    in
      nixpkgs.lib.mapAttrs (Derivation self) (import ./packages.nix {})
      // { inherit (nixpkgs) emacs; };
in
  self
