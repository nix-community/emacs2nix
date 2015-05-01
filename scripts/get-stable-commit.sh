#!/bin/sh

cd $melpa/working/$name

case $fetcher in
    git|github)
        if [[ -a .git ]]; then
            tag=$(git tag -l --sort=v:refname | grep -E "^([Vv][.-]?)?([0-9]+[^ \t\n]*)$" | tail -n1)
            if [[ -n "$tag" ]]; then
                # a stable version exists
                commit=$(git log --first-parent -n1 --pretty=format:'%H' $tag)
                echo "{ \"$name\": \"$commit\" }"
            else
                echo "$name: could not get stable commit" >&2
                echo "{ }"
            fi
        else
            echo "$name: could not get stable commit" >&2
            echo "{ }"
        fi
        ;;
    hg)
        if [[ -a .hg ]]; then
            commit=$(hg tags | perl -ne 'print "$2\n" if /^([Vv][.-]?)?([0-9]+[^ \t\n]*)[ \t]*[0-9]+:([[:xdigit:]]+)/' | head)
            if [[ -n "$commit" ]]; then
                # a stable version exists
                echo "{ \"$name\": \"$commit\" }"
            else
                echo "$name: could not get stable commit" >&2
                echo "{ }"
            fi
        else
            echo "$name: could not get stable commit" >&2
            echo "{ }"
        fi
        ;;
    *)
        echo "{ }"
        ;;
esac
