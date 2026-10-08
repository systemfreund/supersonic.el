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

.PHONY: lint
lint: cask
	cask emacs -Q --batch -L . -l package-lint \
	  --eval '(setq package-lint-main-file "supersonic.el")' \
	  -f package-lint-batch-and-exit $$(cask files)
	@out=$$(cask emacs -Q --batch -L . \
	          --eval '(progn (require (quote checkdoc)) (mapc (function checkdoc-file) command-line-args-left))' \
	          $$(cask files) 2>&1 \
	        | grep -E '^supersonic[^ ]*\.el:[0-9]+: ') ; \
	  if [ -n "$$out" ] ; then echo "$$out" ; exit 1 ; fi ; \
	  echo "checkdoc: clean"

.PHONY: test
test: cask
	cask emacs -Q --batch -L . -l ert -l supersonic.el -l supersonic-tests.el \
		-f ert-run-tests-batch-and-exit

.PHONY: clean
clean:
	cask clean-elc
	rm -f *.elc
