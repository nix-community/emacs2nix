#! /usr/bin/env nix-shell
#! nix-shell shell-fetch.nix -i bash

# usage: ./nongnu-packages.sh -o PATH

elpa2nix https://elpa.nongnu.org/nongnu/ "$@"
