;;; init-langs.el --- per-language setup -*- lexical-binding: t; -*-
;;; Commentary:
;;; Per-language mode setup.
;;; Prefer built-in tree-sitter modes (*-ts-mode) and use eglot for LSP.
;;; treesit-auto, configured in init-lsp.el, installs grammars and remaps modes.
;;; Code:

;;; Save-time formatting, unified through apheleia: asynchronous (no save
;;; blocking on a slow LSP server), keeps point, and dispatches one external
;;; formatter per mode. Replaces prettier-js, eglot-format-buffer hooks,
;;; elm-format-on-save, and rubocop-autocorrect-on-save.
(use-package apheleia
  :diminish apheleia-mode
  :config
  ;; Explicit allowlist instead of the upstream default alist: only modes
  ;; that formatted on save before keep doing so, plus the decided
  ;; overrides (ruff instead of black, rubocop instead of prettier-ruby).
  (setq apheleia-mode-alist
        '((js-ts-mode . prettier-javascript)
          (typescript-ts-mode . prettier-typescript)
          (tsx-ts-mode . prettier-typescript)
          (css-mode . prettier-css)
          (css-ts-mode . prettier-css)
          (scss-mode . prettier-scss)
          (web-mode . prettier)
          (svelte-mode . prettier-svelte)
          (elm-mode . elm-format)
          (python-mode . ruff)
          (python-ts-mode . ruff)
          (ruby-mode . rubocop)
          (ruby-ts-mode . rubocop)
          (rust-ts-mode . rustfmt)
          (go-ts-mode . gofmt)
          (zig-ts-mode . zig-fmt)))
  (apheleia-global-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Clojure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare-function put-clojure-indent "clojure-mode")
(use-package clojure-mode
  :defer t                              ; Autoloads keep auto-mode-alist wired.
  :init
  (add-hook 'clojure-mode-hook #'yas-minor-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'my/lisp-mode-hook)
  :config
  ;; Compojure indentation.
  ;; https://github.com/weavejester/compojure/wiki/Emacs-indentation
  (define-clojure-indent
   (defroutes 'defun)
   (GET 2) (POST 2) (PUT 2) (DELETE 2) (HEAD 2) (ANY 2) (context 2)))

(use-package cider
  :after clojure-mode
  :hook (cider-repl-mode . my/lisp-mode-hook)
  :config
  ;; REPL pretty printing stays on cider-repl-use-pretty-printing's default;
  ;; the old cider-repl-toggle-pretty-printing variable no longer exists.
  (setq nrepl-log-messages t
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t
        cider-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        cider-overlays-use-font-lock t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Scheme ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package geiser
  :defer t            ; Autoloads hook scheme-mode for on-demand activation.
  :init
  (add-hook 'geiser-mode-hook #'my/lisp-mode-hook)
  (add-hook 'geiser-repl-mode-hook #'my/lisp-mode-hook)
  :config
  (setq geiser-active-implementations '(racket)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Common Lisp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sly through Roswell. Load the helper if it exists.
(when (file-exists-p "~/.roswell/helper.el")
  (load (expand-file-name "~/.roswell/helper.el"))
  (with-eval-after-load 'sly
    (add-hook 'sly-mode-hook #'my/lisp-mode-hook)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Ruby ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ruby-mode
  :ensure nil
  :init
  (setq ruby-insert-encoding-magic-comment nil)
  ;; ruby-mode is remapped to ruby-ts-mode by treesit-auto.
  :hook ((ruby-ts-mode . display-line-numbers-mode)
         (ruby-ts-mode . eglot-ensure)))

;;; rubocop-mode keeps the check/project commands; autocorrect-on-save stays
;;; off because apheleia runs rubocop -a as the save-time formatter.
(use-package rubocop
  :hook (ruby-ts-mode . rubocop-mode))

(use-package ruby-electric
  :hook (ruby-ts-mode . ruby-electric-mode))

(use-package ruby-refactor
  :hook (ruby-ts-mode . ruby-refactor-mode-launch))

(use-package slim-mode
  :hook (slim-mode . display-line-numbers-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Python ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package python
  :ensure nil
  :hook (python-ts-mode . eglot-ensure))

(use-package poetry
  :commands (poetry poetry-tracking-mode))

;;; Jupyter notebook
(use-package ein
  :commands (ein:run ein:login))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Rust ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'rust-ts-mode-hook #'eglot-ensure)

(use-package cargo
  :hook (rust-ts-mode . cargo-minor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Zig ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package zig-ts-mode
  :commands zig-ts-mode
  :init
  ;; Prefer zig-ts-mode's grammar management and avoid zig-mode format hooks.
  (dolist (pattern '("\\.zig\\'" "\\.\\(zig\\|zon\\)\\'"))
    (setq auto-mode-alist (assoc-delete-all pattern auto-mode-alist)))
  (add-to-list 'auto-mode-alist '("\\.\\(zig\\|zon\\)\\'" . zig-ts-mode))
  :hook (zig-ts-mode . eglot-ensure))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Go ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'go-ts-mode-hook #'eglot-ensure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; JavaScript / TypeScript ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; js/typescript/tsx use built-in *-ts-mode via treesit-auto associations.
(add-hook 'typescript-ts-mode-hook #'eglot-ensure)
(add-hook 'tsx-ts-mode-hook #'eglot-ensure)
(setopt js-indent-level 2
        typescript-ts-mode-indent-offset 2)

;;; Angular
(use-package ng2-mode
  :commands (ng2-mode ng2-ts-mode ng2-html-mode))

;;; Vue
(use-package vue-mode
  :mode "\\.vue\\'")

;;; CoffeeScript
(use-package coffee-mode
  :hook (coffee-mode . display-line-numbers-mode))

;;; Elm. Save-time formatting comes from apheleia (elm-format), so the
;;; package's own elm-format-on-save stays off to avoid double formatting.
(use-package elm-mode
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Web ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package web-mode
  :mode ("\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.html?\\'" "\\.svelte\\'")
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-style-padding 2
        web-mode-script-padding 2
        web-mode-block-padding 2
        web-mode-enable-engine-detection t))

;;; Stylesheet. scss-mode comes built in via css-mode.el; the MELPA scss-mode
;;; references flymake-allowed-file-name-masks, removed in Emacs 30, and fails
;;; to load. Save-time formatting comes from apheleia (prettier-scss).
(setopt css-indent-offset 2)            ; Shared by css-mode and scss-mode.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; JSON / YAML ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Let treesit-auto promote JSON/YAML files when grammars are ready.
(add-to-list 'auto-mode-alist '("\\.babelrc\\'" . js-json-mode))
(add-hook 'yaml-ts-mode-hook #'display-line-numbers-mode)

;;; JSON formatting with the external jq command.
(defun my/jq-format (beg end)
  "Reformat region (BEG END) by jq."
  (interactive "r")
  (shell-command-on-region beg end "jq ." nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Markdown ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PHP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package php-mode
  :hook (php-mode . eglot-ensure))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Java ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Replace meghanada with eglot and jdtls.
(add-hook 'java-ts-mode-hook #'eglot-ensure)
(add-hook 'java-ts-mode-hook (lambda () (setq c-basic-offset 2)))

(use-package groovy-mode :mode "\\.groovy\\'")
(use-package gradle-mode :commands gradle-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Kotlin ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package kotlin-mode
  :hook (kotlin-mode . eglot-ensure))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Swift ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package swift-mode
  :custom (swift-mode:basic-offset 2)
  :hook (swift-mode . eglot-ensure))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Terraform ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package terraform-mode
  :hook (terraform-mode . eglot-ensure))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Others ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package fish-mode    :mode "\\.fish\\'")
(use-package csv-mode     :mode "\\.csv\\'")
(use-package abc-mode     :mode "\\.abc\\'")
(use-package cmake-mode   :commands cmake-mode)   ; Remapped to cmake-ts-mode.
(use-package bazel        :commands bazel-mode)
(use-package apib-mode    :mode "\\.apib\\'")

;;; dockerfile uses built-in dockerfile-ts-mode via treesit-auto associations.

;;; SQL indentation.
(use-package sql-indent
  :hook (sql-mode . sqlind-minor-mode))

;;; PlantUML. flycheck-plantuml is removed with the flycheck migration.
(use-package plantuml-mode
  :mode ("\\.puml\\'" "\\.plantuml\\'")
  :config
  (setq plantuml-jar-path "~/bin/plantuml.jar"
        plantuml-default-exec-mode 'jar))

(provide 'init-langs)
;;; init-langs.el ends here
