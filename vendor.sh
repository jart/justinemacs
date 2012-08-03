#!/bin/bash
set -x

mkdir -p vendor
cd vendor

[[ ! -d magit ]] && git clone git://github.com/magit/magit.git
cd magit
git pull
cd ..

[[ ! -d yasnippet ]] && git clone git://github.com/capitaomorte/yasnippet.git
cd yasnippet
git pull
cd ..

[[ ! -d coffee-mode ]] && git clone git://github.com/jart/coffee-mode.git
cd coffee-mode
git pull
cd ..

cd ..

emacs --batch --eval "(progn (package-refresh-contents) (package-install 'company))"
emacs --batch --eval '(progn (load-file "init.el") (lob/recompile))'
