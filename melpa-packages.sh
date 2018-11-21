#! /usr/bin/env nix-shell
#! nix-shell shell-fetch.nix -i bash

# usage: ./melpa-packages.sh --melpa PATH_TO_MELPA_CLONE

melpa2nix --names names.nix "$@"
