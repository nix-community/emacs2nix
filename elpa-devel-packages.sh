#! /usr/bin/env nix-shell
#! nix-shell shell-fetch.nix -i bash

# usage: ./elpa-packages.sh -o PATH

elpa2nix https://elpa.gnu.org/devel/ "$@"
