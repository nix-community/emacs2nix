#!/bin/sh

cd $melpa/working/$name

case $fetcher in
    git|github)
        if [[ -a .git ]]; then
            commit=$(git log --first-parent -n1 --pretty=format:'%H')
            echo "{ \"$name\": \"$commit\" }"
        else
            echo "{ }"
        fi
        ;;
    hg)
        if [[ -a .hg ]]; then
            commit=$(hg tags | perl -ne 'print "$1\n" if /^tip[ \t]*[0-9]+:([[:xdigit:]]+)$/' | head)
            if [[ -n "$commit" ]]; then
                # a stable version exists
                echo "{ \"$name\": \"$commit\" }"
            else
                echo "{ }"
            fi
        else
            echo "{ }"
        fi
        ;;
    *)
        echo "{ }"
        echo "$name: fetcher $fetcher unimplemented" >&2
        ;;
esac
