#!/usr/bin/env nix-shell
#!nix-shell -i bash -p haskellPackages.cabal-install nix-prefetch-scripts

# usage: ./melpa-packages.sh PATH_TO_MELPA_CLONE

nix-shell -A env --run "cabal run melpa2nix -- --stable -o melpa-stable-packages.json --work /tmp/melpa-stable2nix --melpa $1"
