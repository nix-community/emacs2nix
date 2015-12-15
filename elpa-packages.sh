#!/usr/bin/env nix-shell
#!nix-shell -i bash -p haskellPackages.cabal-install nix-prefetch-scripts

# usage: ./elpa-packages.sh

nix-shell -A env --run 'cabal run elpa2nix -- -o elpa-packages.json http://elpa.gnu.org/packages/'
