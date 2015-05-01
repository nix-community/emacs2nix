#!/bin/sh

cd $melpa/working/$name

case $fetcher in
    git|github)
        if [[ -a .git ]]; then
            git checkout $branch >/dev/null 2>&1
            commit=$(git log --first-parent -n1 --pretty=format:'%H' $branch 2>/dev/null)
            echo "{ \"$name\": \"$commit\" }"
        else
            echo "$name: could not get commit" >&2
            echo "{ }"
        fi
        ;;
    hg)
        if [[ -a .hg ]]; then
            commit=$(hg tags | perl -ne 'print "$1\n" if /^tip[ \t]*[0-9]+:([[:xdigit:]]+)$/' 2>/dev/null | head)
            if [[ -n "$commit" ]]; then
                echo "{ \"$name\": \"$commit\" }"
            else
                echo "$name: could not get commit" >&2
                echo "{ }"
            fi
        else
            echo "$name: could not get commit" >&2
            echo "{ }"
        fi
        ;;
    svn)
        if [[ -a .svn ]]; then
            commit=$(svn info | perl -ne 'print "$1\n" if /^Revision:[ \t]*([0-9]+)$/' 2>/dev/null)
            if [[ -n "$commit" ]]; then
                echo "{ \"$name\": \"$commit\" }"
            else
                echo "$name: could not get commit" >&2
                echo "{ }"
            fi
        else
            echo "$name: could not get commit" >&2
            echo "{ }"
        fi
        ;;
    *)
        echo "$name: not implemented" >&2
        echo "{ }"
        ;;
esac
