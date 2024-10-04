#! /usr/bin/env nix-shell
#! nix-shell shell-fetch.nix -i bash

# usage: ./jcs-elpa-packages.sh -o PATH

elpa2nix https://jcs-emacs.github.io/jcs-elpa/packages/ "$@"
