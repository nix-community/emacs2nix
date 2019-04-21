# builder for Emacs packages built for packages.el
# using MELPA package-build.el

{
  lib, stdenv,
  fetchgit, fetchhg, fetchurl, fetchFromGitHub, fetchFromGitLab,
  emacs, texinfo, packageBuild,
}:

self:

with lib;

let
  fetchRecipe =
    { ename, rev, sha256 }:
    fetchurl {
      url = "https://raw.githubusercontent.com/melpa/melpa/${rev}/recipes/${ename}";
      name = ename;
      inherit sha256;
    };

  fetch = src:
    src {
      Git = fetchgit;
      GitHub = fetchFromGitHub;
      GitLab = fetchFromGitLab;
      Hg = fetchhg;
      Recipe = fetchRecipe;
      Url = fetchurl;
    };

  mkDep = name: value:
    if value != null then value else self.${name};
in

/* Nix package name */
pname:

{
  /* Emacs package name */
  ename,
  version,
  src,
  recipe,
  deps,
}:

let
  deps' = lib.mapAttrs mkDep deps;
  buildInputs = lib.attrValues deps';
in

stdenv.mkDerivation {
  name = "emacs-${pname}-${version}";
  inherit pname ename version;

  inherit packageBuild;
  elpa2nix = ./elpa2nix.el;
  melpa2nix = ./melpa2nix.el;

  src = fetch src;
  recipe = fetch recipe;

  buildInputs = [ emacs texinfo ] ++ buildInputs;
  propagatedBuildInputs = buildInputs;

  preUnpack = ''
    mkdir -p "$NIX_BUILD_TOP/recipes"
    if [ -n "$recipe" ]; then
      cp "$recipe" "$NIX_BUILD_TOP/recipes/$ename"
    fi

    ln -s "$packageBuild" "$NIX_BUILD_TOP/package-build"

    mkdir -p "$NIX_BUILD_TOP/packages"
  '';

  unpackCmd = ''
    case "$curSrc" in
      *.el)
        # keep original source filename without the hash
        local filename=$(basename "$curSrc")
        filename="''${filename:33}"
        cp $curSrc $filename
        chmod +w $filename
        sourceRoot="."
        ;;
      *)
        _defaultUnpack "$curSrc"
        ;;
    esac
  '';

  postUnpack = ''
    mkdir -p "$NIX_BUILD_TOP/working"
    ln -s "$NIX_BUILD_TOP/$sourceRoot" "$NIX_BUILD_TOP/working/$ename"
  '';

  buildPhase = ''
    runHook preBuild

    cd "$NIX_BUILD_TOP"

    emacs --batch -Q \
        -L "$NIX_BUILD_TOP/package-build" \
        -l "$melpa2nix" \
        -f melpa2nix-build-package \
        $ename $version

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    archive="$NIX_BUILD_TOP/packages/$ename-$version.el"
    if [ ! -f "$archive" ]; then
        archive="$NIX_BUILD_TOP/packages/$ename-$version.tar"
    fi

    emacs --batch -Q \
        -l "$elpa2nix" \
        -f elpa2nix-install-package \
        "$archive" "$out/share/emacs/site-lisp/elpa"

    runHook postInstall
  '';

  meta = {
    homepage = "http://melpa.org/#/${ename}";
    license = lib.licenses.gpl3Plus;
  };
}
