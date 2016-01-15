#!/usr/bin/env nix-shell
#!nix-shell -i bash -A env

# usage: ./melpa-packages.sh --melpa PATH_TO_MELPA_CLONE

cabal run melpa2nix -- --stable --work /tmp/melpa-stable2nix "$@"
