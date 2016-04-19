# emacs2nix

Automatically generate Nix expressions for Emacs packages.

The scripts `elpa-packages.sh`, `org-packages.sh`, `melpa-packages.sh`, and
`melpa-stable-packages.sh` regenerate each package set. They require
Nix to build. The first argument to the MELPA scripts should be the
path to a clone of the [MELPA](https://github.com/milkypostman/melpa)
repository.

To run one of the `elpa2nix` or `melpa2nix` commands outside the shell script
wrappers, first enter a Nix shell with the command
```
nix-shell -A env
```
which will ensure that all the runtime dependencies are present.
