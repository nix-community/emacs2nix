#! /usr/bin/env nix-shell
#! nix-shell shell-fetch.nix -i bash

# usage: ./org-packages.sh -o PATH

elpa2nix http://orgmode.org/elpa/ "$@"
