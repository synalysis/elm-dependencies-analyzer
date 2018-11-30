#!/bin/bash

#
# This little script is originally from
# https://guide.elm-lang.org/optimization/asset_size.html
#

set -e

SOURCES=src/*.elm
JS_TEMP=elm.tmp.js
JS_TARGET=elm.js


if [ "$1" = "" ]
then
    elm make $SOURCES --optimize --output=$JS_TEMP

    uglifyjs $JS_TEMP --compress \
      'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
      2>/dev/null | uglifyjs --mangle --output=$JS_TARGET

    echo
    echo "Compiled size: $(cat $JS_TEMP             | wc -c) bytes"
    echo "Minified size: $(cat $JS_TARGET           | wc -c) bytes"
    echo "Gzipped  size: $(cat $JS_TARGET | gzip -c | wc -c) bytes"

    rm $JS_TEMP

elif [ "$1" = "d" ]
then
    elm make $SOURCES --debug --output=$JS_TARGET

elif [ "$1" = "q" ]
then
    elm make $SOURCES --optimize --output=$JS_TARGET

elif [ "$1" = "qq" ]
then
    elm make $SOURCES --output=$JS_TARGET

else
    echo
    echo "USAGE"
    echo "  ./make.sh [d | q | qq]"
    echo
    echo "OPTIONS"
    echo "  d   debug         ; with --debug"
    echo "  q   quick         ; no minifying"
    echo "  qq  really quick  ; no minifying or --optimize"
    echo
    echo
    echo
fi
