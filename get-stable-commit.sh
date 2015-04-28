#!/bin/sh

cd $melpa/working/$name

case $fetcher in
    git|github)
        if [[ -a .git ]]; then
            tag=$(git tag -l | grep -E "^(?:v[.-]?)?([0-9]+[^ \t\n]*)$" | tail -n1)
            if [[ -n "$tag" ]]; then
                # a stable version exists
                commit=$(git log --first-parent -n1 --pretty=format:'%H' $tag)
                echo "{ \"$name\": \"$commit\" }"
            else
                echo "{ }"
            fi
        else
            echo "{ }"
        fi
        ;;
    hg)
        if [[ -a .hg ]]; then
            commit=$(hg tags | perl -ne 'print "$2\n" if /^(?:v[.-]?)?([0-9]+[^ \t\n]*)[ \t]*[0-9]+:([[:xdigit:]]+)/' | head)
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
        echo "$name: stable fetcher $fetcher unimplemented" >&2
        ;;
esac
