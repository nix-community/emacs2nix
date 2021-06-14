# emacs2nix

Automatically generate Nix expressions for Emacs packages.

Be sure to run
```
git submodule update --init
```
before building this package!

The scripts `elpa-packages.sh`, `nongnu-packages.sh`, `org-packages.sh`,
`melpa-packages.sh`, and `melpa-stable-packages.sh` regenerate each package
set. They require Nix to build.

To update the ELPA or org-mode packages, run
```.bash
# For ELPA packages
./elpa-packages.sh \
  -o $NIXPKGS/pkgs/applications/editors/emacs-modes/elpa-generated.nix \
  --names names.nix
# For org-mode packages
./org-packages.sh \
  -o $NIXPKGS/pkgs/applications/editors/emacs-modes/org-generated.nix \
  --names names.nix
```
`$NIXPKGS` should be the path to the Nixpkgs clone which you are updating.

To update the MELPA packages, run
```.bash
# Use melpa-stable-packages.sh to update melpa-stable-generated.nix instead
./melpa-packages.sh \
  --melpa $MELPA \
  -o $NIXPKGS/pkgs/applications/editors/emacs-modes/melpa-generated.nix \
  --names names.nix
```
`$MELPA` should be tho path to a clone of the [MELPA](https://github.com/milkypostman/melpa)
repository.

To run one of the `elpa2nix` or `melpa2nix` commands outside the shell script
wrappers, first enter a Nix shell with the command
```
nix-shell -A env
```
which will ensure that all the runtime dependencies are present.
