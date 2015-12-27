#!/usr/bin/env nix-shell
#!nix-shell -i bash -A env

# usage: ./melpa-packages.sh --melpa PATH_TO_MELPA_CLONE

cabal run melpa2nix -- -o melpa-packages.json --work /tmp/melpa2nix "$@"
