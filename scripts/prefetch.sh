#!/bin/sh

case $fetcher in
    git)
        hash=$(QUIET=1 $nixpkgs/pkgs/build-support/fetchgit/nix-prefetch-git --url $url --rev $commit 2>/dev/null | tail -n1)
        if [[ -n "$hash" ]]; then
            echo "{ \"$name\": \"$hash\" }"
        else
            echo "{ }"
        fi
        ;;
    github)
        hash=$(nix-prefetch-url "https://github.com/${repo}/archive/${commit}.tar.gz" 2>/dev/null)
        if [[ -n "$hash" ]]; then
            echo "{ \"$name\": \"$hash\" }"
        else
            echo "{ }"
        fi
        ;;
    hg)
        hash=$(QUIET=1 $nixpkgs/pkgs/build-support/fetchhg/nix-prefetch-hg $url $commit 2>/dev/null)
        if [[ -n "$hash" ]]; then
            echo "{ \"$name\": \"$hash\" }"
        else
            echo "{ }"
        fi
        ;;
    svn)
        hash=$(QUIET=1 $nixpkgs/pkgs/build-support/fetchsvn/nix-prefetch-svn $url $commit 2>/dev/null)
        if [[ -n "$hash" ]]; then
            echo "{ \"$name\": \"$hash\" }"
        else
            echo "{ }"
        fi
        ;;
    wiki)
        if [[ -n "$url" ]]; then
            hash=$(nix-prefetch-url $url 2>/dev/null)
            if [[ -n "$hash" ]]; then
                echo "{ \"$name\": \"$hash\" }"
            else
                echo "{ }"
            fi
        else
            hash=$(nix-prefetch-url "http://www.emacswiki.org/emacs/download/$name.el" 2>/dev/null)
            if [[ -n "$hash" ]]; then
                echo "{ \"$name\": \"$hash\" }"
            else
                echo "{ }"
            fi
        fi
        ;;
    cvs)
        if [[ -z "$branch" ]]; then
            branch=$name
        fi
        hash=$(QUIET=1 $nixpkgs/pkgs/build-support/fetchcvs/nix-prefetch-cvs $url $branch 2>/dev/null)
        if [[ -n "$hash" ]]; then
            echo "{ \"$name\": \"$hash\" }"
        else
            echo "{ }"
        fi
        ;;
    bzr)
        hash=$(QUIET=1 $nixpkgs/pkgs/build-support/fetchbzr/nix-prefetch-bzr $url $commit 2>/dev/null)
        if [[ -n "$hash" ]]; then
            echo "{ \"$name\": \"$hash\" }"
        else
            echo "{ }"
        fi
        ;;
    *)
        echo "{ }"
        ;;
esac
