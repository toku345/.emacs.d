EMACS ?= emacs

ELISP_FILES := \
	early-init.el \
	lisp/init-package.el \
	lisp/init-core.el \
	lisp/init-keys.el \
	lisp/init-completion.el \
	lisp/init-ui.el \
	lisp/init-editing.el \
	lisp/init-vc.el \
	lisp/init-project.el \
	lisp/init-lsp.el \
	lisp/init-langs.el \
	lisp/init-org.el \
	lisp/init-misc.el \
	init.el \
	scripts/checkdoc-batch.el \
	scripts/byte-compile-batch.el \
	test/init-package-test.el \
	test/init-langs-test.el

.PHONY: check whitespace smoke checkdoc test byte-compile package-quickstart clean-elc

check: whitespace smoke checkdoc test byte-compile

whitespace:
	git diff --check
	git diff --cached --check

smoke:
	$(EMACS) -Q --batch -l early-init.el -l init.el

checkdoc:
	$(EMACS) -Q --batch --eval "(progn (load-file \"scripts/checkdoc-batch.el\") (my/checkdoc-batch-run))" -- $(ELISP_FILES)

# ERT tests run against the fully loaded configuration so package
# activation (and any stale shadowing autoloads) is in effect.
test:
	$(EMACS) -Q --batch -l early-init.el -l init.el -l test/init-package-test.el -l test/init-langs-test.el -f ert-run-tests-batch-and-exit

byte-compile:
	$(EMACS) -Q --batch -L lisp -l lisp/init-package.el --eval "(progn (load-file \"scripts/byte-compile-batch.el\") (my/byte-compile-batch-run))" -- $(ELISP_FILES)

package-quickstart:
	$(EMACS) -Q --batch -l early-init.el -l lisp/init-package.el --eval "(package-quickstart-refresh)"

clean-elc:
	find . -name '*.elc' -type f -delete
