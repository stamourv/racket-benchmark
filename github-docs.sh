#!/bin/bash

# run from this directory
scribble --html --dest . --dest-name index.html +m --redirect-main http://docs.racket-lang.org benchmark/scribblings/benchmark.scrbl
git checkout gh-pages
git add *.html *.css *.js *.png
git commit -am 'Update github docs.'
git push
git checkout master
