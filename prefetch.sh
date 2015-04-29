#!/bin/sh

case $fetcher in
    git)
        hash=$($nixpkgs/pkgs/build-support/fetchgit/nix-prefetch-git --url $url --rev $commit)
        if [[ -n "$hash" ]]; then
            echo "{ \"$name\": \"$hash\" }"
        else
            echo "{ }"
        fi
        ;;
    github)
        hash=$($nixpkgs/pkgs/build-support/fetchgit/nix-prefetch-git --url https://github.com/$repo --rev $commit)
        if [[ -n "$hash" ]]; then
            echo "{ \"$name\": \"$hash\" }"
        else
            echo "{ }"
        fi
        ;;
    hg)
        hash=$($nixpkgs/pkgs/build-support/fetchhg/nix-prefetch-hg $url $commit)
        if [[ -n "$hash" ]]; then
            echo "{ \"$name\": \"$hash\" }"
        else
            echo "{ }"
        fi
        ;;
    *)
        echo "{ }"
        echo "$name: fetcher $fetcher unimplemented" >&2
        ;;
esac
