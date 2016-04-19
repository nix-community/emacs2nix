#!/usr/bin/env nix-shell
#!nix-shell -i bash -A env

# usage: ./org-packages.sh -o PATH

## env var for curl
export SSL_CERT_FILE=${SSL_CERT_FILE:+/etc/ssl/certs/ca-certificates.crt}

cabal run elpa2nix -- http://orgmode.org/elpa/ "$@"
