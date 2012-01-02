#!/bin/bash
set -x

mkdir -p vendor
cd vendor

emacs --batch --eval \
    "(progn (package-refresh-contents) (package-install 'company))"

[[ ! -d magit ]] && git clone git://github.com/magit/magit.git
cd magit
git pull
cd ..

[[ ! -d yasnippet ]] && git clone git://github.com/capitaomorte/yasnippet.git
cd yasnippet
git pull
cd ..
