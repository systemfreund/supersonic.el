EMACS ?= emacs

.PHONY: compile test clean

compile:
	$(EMACS) -Q --batch -L . -f batch-byte-compile subsonic.el subsonic-mpris.el

test:
	$(EMACS) -Q --batch -L . -l ert -l subsonic.el -l subsonic-tests.el \
		-f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc
