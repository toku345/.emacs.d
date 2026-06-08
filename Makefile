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
	scripts/byte-compile-batch.el

.PHONY: check whitespace smoke checkdoc byte-compile clean-elc

check: whitespace smoke checkdoc byte-compile

whitespace:
	git diff --check
	git diff --cached --check

smoke:
	$(EMACS) --batch -l init.el

checkdoc:
	$(EMACS) -Q --batch --eval "(progn (load-file \"scripts/checkdoc-batch.el\") (my/checkdoc-batch-run))" -- $(ELISP_FILES)

byte-compile:
	$(EMACS) -Q --batch --eval "(progn (load-file \"scripts/byte-compile-batch.el\") (my/byte-compile-batch-run))" -- $(ELISP_FILES)

clean-elc:
	find . -name '*.elc' -type f -delete
