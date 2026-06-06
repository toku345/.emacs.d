;;; init-langs.el --- per-language setup -*- lexical-binding: t; -*-
;;; Commentary:
;;; 各プログラミング言語のモード設定。
;;; 方針: 組込 tree-sitter モード（*-ts-mode）を使い、LSP は eglot に統一。
;;;       treesit-auto（init-lsp.el）が文法導入と従来モードからの remap を担う。
;;; Code:

;;; 保存時に eglot で整形するためのヘルパー
(defun my/eglot-format-on-save ()
  "現在バッファの保存前に eglot-format-buffer を実行する。"
  (add-hook 'before-save-hook #'eglot-format-buffer nil t))

;;; 整形ツール（外部 prettier コマンド）
(use-package prettier-js
  :commands prettier-js-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Clojure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package clojure-mode
  :init
  (add-hook 'clojure-mode-hook #'yas-minor-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'my/lisp-mode-hook)
  :config
  ;; Compojure 用インデント
  ;; https://github.com/weavejester/compojure/wiki/Emacs-indentation
  (define-clojure-indent
   (defroutes 'defun)
   (GET 2) (POST 2) (PUT 2) (DELETE 2) (HEAD 2) (ANY 2) (context 2)))

(use-package cider
  :after clojure-mode
  :hook (cider-repl-mode . my/lisp-mode-hook)
  :config
  (setq nrepl-log-messages t
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t
        cider-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        cider-overlays-use-font-lock t
        cider-repl-toggle-pretty-printing t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Scheme ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package geiser
  :init
  (add-hook 'geiser-mode-hook #'my/lisp-mode-hook)
  (add-hook 'geiser-repl-mode-hook #'my/lisp-mode-hook)
  :config
  (setq geiser-active-implementations '(racket)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Common Lisp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Roswell 経由の sly。helper があれば読み込む。
(when (file-exists-p "~/.roswell/helper.el")
  (load (expand-file-name "~/.roswell/helper.el"))
  (with-eval-after-load 'sly
    (add-hook 'sly-mode-hook #'my/lisp-mode-hook)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Ruby ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ruby-mode
  :ensure nil
  :init
  (setq ruby-insert-encoding-magic-comment nil)
  ;; ruby-mode は treesit-auto により ruby-ts-mode へ remap される
  :hook ((ruby-ts-mode . display-line-numbers-mode)
         (ruby-ts-mode . yafolding-mode)
         (ruby-ts-mode . eglot-ensure)))

(use-package rubocop
  :hook (ruby-ts-mode . rubocop-mode)
  :custom (rubocop-autocorrect-on-save t))

(use-package ruby-electric
  :hook (ruby-ts-mode . ruby-electric-mode))

(use-package ruby-refactor
  :hook (ruby-ts-mode . ruby-refactor-mode-launch))

(use-package slim-mode
  :hook (slim-mode . display-line-numbers-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Python ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package python
  :ensure nil
  :hook ((python-ts-mode . eglot-ensure)
         (python-ts-mode . my/eglot-format-on-save)))

(use-package poetry
  :commands (poetry poetry-tracking-mode))

;;; Jupyter notebook
(use-package ein
  :commands (ein:run ein:login))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Rust ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rust-mode
  :ensure nil
  :custom (rust-format-on-save t)
  :hook (rust-ts-mode . eglot-ensure))

(use-package cargo
  :hook (rust-ts-mode . cargo-minor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Go ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package go-mode
  :ensure nil
  :hook ((go-ts-mode . eglot-ensure)
         (go-ts-mode . my/eglot-format-on-save)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; JavaScript / TypeScript ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; js/typescript/tsx は組込 *-ts-mode を使用（treesit-auto が remap）
(add-hook 'js-ts-mode-hook #'prettier-js-mode)
(add-hook 'typescript-ts-mode-hook #'eglot-ensure)
(add-hook 'typescript-ts-mode-hook #'prettier-js-mode)
(add-hook 'tsx-ts-mode-hook #'eglot-ensure)
(add-hook 'tsx-ts-mode-hook #'prettier-js-mode)
(setq js-indent-level 2
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

;;; Elm
(use-package elm-mode
  :init (setq elm-format-on-save t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Web ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package web-mode
  :mode ("\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.html?\\'" "\\.svelte\\'")
  :hook (web-mode . prettier-js-mode)
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-style-padding 2
        web-mode-script-padding 2
        web-mode-block-padding 2
        web-mode-enable-engine-detection t))

;;; Stylesheet
(use-package scss-mode
  :hook (css-mode . prettier-js-mode)
  :config
  (setq css-indent-offset 2
        scss-compile-at-save nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; JSON / YAML ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; json-ts-mode / yaml-ts-mode は組込（treesit-auto が remap）
(add-to-list 'auto-mode-alist '("\\.babelrc\\'" . json-ts-mode))
(add-hook 'yaml-ts-mode-hook #'display-line-numbers-mode)

;;; jq による JSON 整形（外部 jq コマンド）
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
;;; meghanada は廃し、eglot（jdtls）に統一
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
(use-package cmake-mode   :commands cmake-mode)   ; cmake-ts-mode へ remap
(use-package bazel        :commands bazel-mode)
(use-package apib-mode    :mode "\\.apib\\'")

;;; dockerfile は組込 dockerfile-ts-mode（treesit-auto が remap）
;;; SQL のインデント
(use-package sql-indent
  :hook (sql-mode . sqlind-minor-mode))

;;; PlantUML（flycheck-plantuml は flycheck 廃止に伴い削除）
(use-package plantuml-mode
  :mode ("\\.puml\\'" "\\.plantuml\\'")
  :config
  (setq plantuml-jar-path "~/bin/plantuml.jar"
        plantuml-default-exec-mode 'jar))

(provide 'init-langs)
;;; init-langs.el ends here
