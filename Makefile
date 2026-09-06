EMACS ?= emacs

.PHONY: compile test clean

compile:
	$(EMACS) -Q --batch -L . -L vendor -f batch-byte-compile supersonic.el supersonic-mpris.el

test:
	$(EMACS) -Q --batch -L . -L vendor -l ert -l supersonic.el -l supersonic-tests.el \
		-f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc
