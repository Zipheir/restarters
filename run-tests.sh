#!/bin/sh

usage () {
        printf 'Usage: %s (chez|guile|chibi)\n' "$(basename $0)" 1>&2
        exit 1
}

test $# -ne 1 && usage

case "$1" in
  chez)
        scheme restarts-chez.sls restarts-test-chez.scm
        ;;
  guile)
        guile --fresh-auto-compile restarts-guile.scm restarts-test-guile.scm
        ;;
  chibi)
        chibi-scheme restarts-test-chibi.scm
        ;;
  -h|--help)
        usage
        ;;
  *)
        echo 'Unknown implementation keyword.' 1>&2
        ;;
esac
