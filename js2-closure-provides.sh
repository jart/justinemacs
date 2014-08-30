#!/bin/bash
# js2-closure --- Provided namespace index generator.
# Author: Justine Tunney <jart@google.com>

( printf "(setq js2-closure-provides '("
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
) >"$(dirname $0)/js2-closure-provides.el"
