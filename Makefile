export EMACS ?= $(shell command -v emacs 2>/dev/null)
CASK_DIR := $(shell cask package-directory)

$(CASK_DIR): Cask
	cask install
	@touch $(CASK_DIR)

.PHONY: cask
cask: $(CASK_DIR)

.PHONY: compile
compile: cask
	cask emacs -Q --batch -L . -f batch-byte-compile $$(cask files); \
	  (ret=$$? ; cask clean-elc && exit $$ret)

.PHONY: test
test: cask
	cask emacs -Q --batch -L . -l ert -l supersonic.el -l supersonic-tests.el \
		-f ert-run-tests-batch-and-exit

.PHONY: clean
clean:
	cask clean-elc
	rm -f *.elc
