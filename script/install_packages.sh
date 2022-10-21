#!/bin/bash

TMPF=$(mktemp)
ag --nofilename --nonumber require-package ./lisp --ignore=lisp/init-package.el | \
        ag -v defun | \
	ag -v '^;;' | \
	cat lisp/init-package.el - >${TMPF}
emacs --script ${TMPF}

rm ${TMPF}
