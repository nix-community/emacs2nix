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
    wiki)
        if [[ -n "$url" ]]; then
            hash=$(nix-prefetch-url $url)
            if [[ -n "$hash" ]]; then
                echo "{ \"$name\": \"$hash\" }"
            else
                echo "{ }"
            fi
        else
            hash=$(nix-prefetch-url "http://www.emacswiki.org/emacs/download/$name.el")
            if [[ -n "$hash" ]]; then
                echo "{ \"$name\": \"$hash\" }"
            else
                echo "{ }"
            fi
        fi
    *)
        echo "{ }"
        echo "$name: fetcher $fetcher unimplemented" >&2
        ;;
esac
