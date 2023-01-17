#!/bin/bash

# elpa/: default elpa working dir
# elpa.save/: saved state before upgrading
# elpa.archive.${curr_time}/: elpa.save archive after upgrade done

if [ ! -e elpa.save ]
then
	mv elpa elpa.save
fi

mkdir -p elpa

TMPF=./install.list
grep -E --no-filename "\(require-package '.*?\)" -R ./lisp --exclude='./lisp/.*' --exclude='lisp/init-package.el' | \
	grep -v defun | \
	grep -v '^;;' | \
	cat lisp/init-package.el - >${TMPF}

# this command eats ctrl-c, make early return explicit
emacs --script ${TMPF} || exit $?

mv elpa.save elpa.archive.$(date +%Y%m%d-%H%M%S)

rm ${TMPF}
