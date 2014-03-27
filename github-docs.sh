#!/bin/bash

# run from this directory
mkdir -p scratch-doc
scribble --html --dest scratch-doc --dest-name index.html +m --redirect-main http://docs.racket-lang.org benchmark/scribblings/benchmark.scrbl
git checkout gh-pages
mv scratch-doc/* .
git add *.html *.css *.js *.png
git commit -am 'Update github docs.'
git push
git checkout master
