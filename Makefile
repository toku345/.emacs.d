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

.PHONY: check whitespace coverage smoke checkdoc byte-compile upgrade clean-elc

check: whitespace coverage smoke checkdoc byte-compile

# Fail when a lisp/*.el module is missing from the hand-maintained
# ELISP_FILES list above; otherwise it silently skips checkdoc/byte-compile.
coverage:
	@missing="$(filter-out $(ELISP_FILES),$(wildcard lisp/*.el))"; \
	if [ -n "$$missing" ]; then \
		echo "ELISP_FILES is missing: $$missing"; \
		exit 1; \
	fi

whitespace:
	git diff --check
	git diff --cached --check

smoke:
	$(EMACS) -Q --batch -l early-init.el -l init.el

checkdoc:
	$(EMACS) -Q --batch --eval "(progn (load-file \"scripts/checkdoc-batch.el\") (my/checkdoc-batch-run))" -- $(ELISP_FILES)

byte-compile:
	$(EMACS) -Q --batch -L lisp -l lisp/init-package.el --eval "(progn (load-file \"scripts/byte-compile-batch.el\") (my/byte-compile-batch-run))" -- $(ELISP_FILES)

# Refresh archives and upgrade every installed package. Most packages track
# rolling MELPA snapshots, so always run make check afterwards (see README).
upgrade:
	$(EMACS) -Q --batch -L lisp -l lisp/init-package.el --eval "(progn (package-refresh-contents) (package-upgrade-all))"

clean-elc:
	find . -name '*.elc' -type f -delete
