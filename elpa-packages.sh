#!/usr/bin/env nix-shell
#!nix-shell -i bash

# usage: ./elpa-packages.sh -o PATH

cabal run elpa2nix -- https://elpa.gnu.org/packages/ "$@"
