#!/usr/bin/env nix-shell
#!nix-shell --pure -i bash -A env

# usage: ./org-packages.sh -o PATH

cabal run elpa2nix -- http://orgmode.org/elpa/ "$@"
