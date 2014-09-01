#!/bin/bash
# js2-closure --- Provided namespace index generator.
# Author: Justine Tunney <jart@google.com>

if [[ $# < 1 || $1 == "-h" || $1 == "--help" ]]; then
  printf "Usage: $0 DIR1 [DIR2...] >~/.emacs.d/js2-closure-provides.sh"
  exit 1
fi

printf "(setq js2-closure-provides '("
while read name; do
  if [[ "${name}" =~ i18n.*_ ]]; then
    continue
  fi
  printf "\n  (%s)" "${name//./ }"
done < <(
  find "$@" -name \*.js -and -not -name \*_test.js \
    | xargs grep -Poh "(?<=^goog.provide\(')[^']+" \
    | sort -u)
printf "))\n"
