;;; init.el --- initialize emacs
;;; -*- mode: Emacs-Lisp ; Coding: utf-8 -*-
;;; Commentary:
;;; This is init.el.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; setup package.el
;;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/") t)

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(when (not (require 'use-package nil t))
  (package-install 'use-package))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; basic configurations
;;;

(set-language-environment "UTF-8")


;;; enable use-package
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-always-pin "melpa-stable")
(setq use-package-verbose t)

;;; setup exec-path
;;; https://github.com/purcell/exec-path-from-shell/blob/master/exec-path-from-shell.el#L88-L89
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))


;;; Move Current Line Up or Down
;;; http://emacsredux.com/blog/2013/04/02/move-current-line-up-or-down/
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;;; global keymap
(use-package bind-key
  :config
  (bind-keys :map global-map
             ("C-h" . delete-backward-char)
             ("C-m" . newline-and-indent)
             ("C-c l" . toggle-truncate-lines)
             ("C-t" . other-window)
             ("C-x ?" . help-command)
             ("C-x SPC" . cua-rectangle-mark-mode)
             ("M-n" . (lambda () (interactive) (scroll-up 1))) ; http://dev.classmethod.jp/devenv/emacs-settings/
             ("M-p" . (lambda () (interactive) (scroll-down 1)))
             ("M-[" . switch-to-prev-buffer)
             ("M-]" . switch-to-next-buffer)
             ("C-c C-p" . move-line-up)
             ("C-c C-n" . move-line-down)))

;;; delete selection mode
(delete-selection-mode t)

;;; GUI only
(when (memq window-system '(mac ns))
  (progn
    (server-start)))

;;; C-k (kill-line) => kill entire line (with return code)
;;; https://emacs.stackexchange.com/a/2348
(setq kill-whole-line t)

;;; Split `custom-set-variables`
(setq custom-file (locate-user-emacs-file "custom.el"))

;;; magit
(use-package magit
  :bind (("C-x M-g" . magit-dispatch-popup)
         ("C-c m" . magit-status)
         ("C-c b" . magit-blame))
  :pin melpa-stable)

;;; elscreen
(use-package elscreen
  :pin melpa
  :init
  (elscreen-start))

;;; flycheck
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

;;; mykie
(use-package mykie
  :config
  (setq mykie:use-major-mode-key-override t)
  (mykie:initialize))

;;; happy (((()))) !!!
(use-package paren
  :init
  (setq show-paren-style 'parenthesis)
  (show-paren-mode 1))

;;; yasnippet
(use-package yasnippet
  :init
  (yas-global-mode 1)
  (bind-keys :map yas-minor-mode-map
             ("<tab>" . nil)
             ("TAB" . nil)
             ("C-i" . nil)
             ("C-o" . yas/expand)))

;;; company-mode
(use-package company
  :config
  (global-company-mode)
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        ;; company-show-numbers t
        company-global-modes '(not magit-status-mode))

  ;; (bind-keys :map company-mode-map
  ;;      ("C-i" . company-complete)) ; C-i & tab で候補を表示
  (bind-keys :map company-active-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)
             ("C-s" . company-search-words-regexp))
  (bind-keys :map company-search-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)))

;;; projectile
(use-package projectile
  :init
  (projectile-global-mode 1))

;;; subword
(use-package subword
  :init
  (global-subword-mode 1))

;;; answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;;; turn off graphical user interface
(dolist (mode '(tool-bar-mode
                ;; menu-bar-mode
                scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;;; summarye
(use-package summarye
  :config
  (bind-keys :map global-map
             ("C-o" . se/make-summary-buffer))
  :pin melpa)

;;; undo-tree
(use-package undo-tree
  :pin melpa
  :config
  (global-undo-tree-mode t)
  (bind-keys :map global-map
             ("C-'" . redo)))

;;; ediff
(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally))

;;; dired
(setq dired-dwim-target t ; diredを2つのウィンドウで開いている時に、デフォルトの移動orコピー先をもう一方のdiredで開いているディレクトリにする
      dired-recursive-copies 'always
      dired-isearch-filenames t
      dired-use-ls-dired t)

;; macOS の `ls` コマンドだとオプションに対応していないので coreutils内の `gls` を使うようにする
;; http://qiita.com/maangie/items/5a80ae50c13d14368a72
(let ((gls "/usr/local/bin/gls"))
  (if (file-exists-p gls) (setq insert-directory-program gls)))

;;; git-gutter
(use-package git-gutter
  :bind (("C-x C-g" . git-gutter:toggle)
         ("C-x v =" . git-gutter:popup-hunk)
         ;; Jump to next/previous hunk
         ("C-x p" . git-gutter:previous-hunk)
         ("C-x n" . git-gutter:next-hunk)
         ;; Stage current hunk
         ("C-x v s" . git-gutter:stage-hunk)
         ;; Revert current hunk
         ("C-x v r" . git-gutter:revert-hunk)
         ;; Mark current hunk
         ("C-x v SPC" . git-gutter:mark-hunk))
  :init
  (global-git-gutter-mode t))

;;; paradox
(use-package paradox
  :config
  (setq paradox-github-token (getenv "PARADOX_GITHUB_TOKEN")))

;; ;;; quelpa
;; (use-package quelpa
;;   :init
;;   (package-initialize)
;;   (if (require 'quelpa nil t)
;;       (quelpa-self-upgrade)
;;     (with-temp-buffer
;;       (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
;;       (eval-buffer))))

;;; executable-make-buffer-file-executable-if-script-p
;; ファイル名が #! から始まる場合、+xをつけて保存する
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;; global-auto-revert-mode
(global-auto-revert-mode 1)

;;; edit-server
(use-package edit-server
  :init
  (edit-server-start)
  :config
  (setq edit-server-new-frame nil))

;;; multiple-cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; emacs appearance settings
;;;

;;; load my preferred theme
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

;;; some useful settings
(setq visible-bell t
      font-lock-maximum-decoration t
      truncate-partial-width-windows nil
      echo-keystrokes 0.1
      create-lockfiles nil
      ;; disable to backup function
      backup-inhibited t
      delete-auto-save-files t
      ;; completion ignore case (lower/upper)
      completion-ignore-case t
      read-file-name-completion-ignore-case t
      inhibit-startup-message t
      frame-title-format "%f")

;;; show me empty lines after buffer end
(set-default 'indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'right)
(setq uniquify-buffer-name-style 'post-forward)

;;; whitespace
(use-package whitespace
  :config
  (setq whitespace-style '(face
                           trailing
                           tabs
                           spaces
                           empty
                           space-mark
                           tab-mark))
  (setq whitespace-display-mappings
        '((space-mark ?\u3000 [?\u25a1])
          (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
  (setq whitespace-space-regexp "\\(\x3000+\\|^ +\\| +$\\)")
  (global-whitespace-mode 1)
  (setq-default tab-width 2 indent-tabs-mode nil))

;;; cleanup whitespace before file save
(add-hook 'before-save-hook 'whitespace-cleanup)

;;; hl-line
(use-package hl-line
  :config
  (setq global-hl-line-timer
        (run-with-idle-timer 0.03 t #'global-hl-line-timer-function))
  (set-face-background 'hl-line "#525252"))

(defun global-hl-line-timer-function ()
  (global-hl-line-unhighlight-all)
  (let ((global-hl-line-mode t))
    (global-hl-line-highlight)))

;;; highlight-symbol
(use-package highlight-symbol
  :config
  (setq highlight-symbol-idle-delay 0.1))


;;; region background color
(set-face-background 'region "dark green")

;;; modeline
(display-time-mode t)

;; show line nums & word count in region when the range specification
(add-to-list 'default-mode-line-format
             '(:eval (count-lines-and-chars)))

;; http://d.hatena.ne.jp/sonota88/20110224/1298557375
(defun count-lines-and-chars ()
  (if mark-active
      (format "%d lines, %d chars "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))
    ;; これだとエコーエリアがチラつく
    ;; (count-lines-region (region-beginning) (region-end))
    ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; searching
;;;

;; ;; Need install cmigemo
;; ;; `$ brew install cmigemo`
;; (use-package migemo
;;   :config
;;   (setq migemo-command "cmigemo"
;;         migemo-options '("-q" "--emacs")
;;         migemo-dictionary "/usr/local/Cellar/cmigemo/20110227/share/migemo/utf-8/migemo-dict"
;;         migemo-user-dictionary nil
;;         migemo-regex-dictionary nil
;;         migemo-coding-system 'utf-8-unix)
;;   (migemo-init))

(use-package ivy
  :config
  ; dired 使いにくくなるのでコメントアウト
  ;; (ivy-mode 1)
  )

(use-package swiper
  :bind (("C-s" . swiper)
         ("C-c C-r" . ivy-resume))
  :init
  (setq ivy-use-virtual-buffers t))

;; (use-package avy-migemo
;;   :config
;;   (avy-migemo-mode 1)
;;   (require 'avy-migemo-e.g.swiper))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; helm
;;;

(use-package helm-ag
  :config
  (setq helm-ag-base-command "ag --nocolor --nogrou --ignore-case"
        helm-ag-thing-at-point 'symbol)
  :bind (("C-c s" . helm-ag)
         ("C-c t" . helm-ag-this-file)))

(use-package helm-ls-git
  :bind ("C-x C-d" . helm-browse-project))

(use-package helm-descbinds
  :bind ("C-c C-b C-b" . helm-descbinds)
  :init (helm-descbinds-mode))

(use-package helm-swoop
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all))
  :config
  (bind-keys :map isearch-mode-map
             ("M-i" . helm-swoop-from-isearch)
             :map helm-swoop-map
             ("M-i" . helm-multi-swoop-all-from-helm-swoop)
             ("M-m" . helm-multi-swoop-current-mode-from-helm-swoop)
             ("C-r" . helm-previous-line)
             ("C-s" . helm-next-line)
             :map helm-multi-swoop-map
             ("C-r" . helm-previous-line)
             ("C-s" . helm-next-line))

  (setq helm-multi-swoop-edit-save t ; Save buffer when helm-multi-swoop-edit complete
        helm-swoop-split-with-multiple-windows nil ; If this value is t, split window inside the current window
        helm-swoop-split-direction 'split-window-vertically ; Split direcion. 'split-window-vertically or 'split-window-horizontally
        helm-swoop-speed-or-color nil ; If nil, you can slightly boost invoke speed in exchange for text color
        helm-swoop-move-to-line-cycle t ; Go to the opposite side of line from the end or beginning of line
        )
  (helm-migemo-mode 1))

(use-package helm-ghq)

(use-package helm
  :config
  (setq helm-quick-update t
        helm-buffers-fuzzy-matching t
        helm-ff-transformer-show-only-basename nil)

  (bind-keys :map global-map
             ("M-x" . helm-M-x)
             ("M-y" . helm-show-kill-ring)
             ;; ("C-x b" . helm-buffers-list)
             ;; ("C-x C-f" . helm-find-files) ; helm-projectile側で設定する
             ("C-x C-b" . helm-for-files)
             ("C-c o" . helm-occur))
  ;; (bind-keys :map helm-find-files-map
  ;;            ("C-h" . delete-backward-char)
  ;;            ("TAB" . helm-execute-persistent-action)) ; ここだと void helm-find-files-map と怒られるので別途設定する
  (bind-keys :map helm-map
             ("C-h" . delete-backward-char)))

(use-package helm-projectile
  :config
  (mykie:set-keys nil
    "C-x C-f"
    :default (call-interactively 'helm-find-files)
    :C-u helm-projectile-find-file
    "C-x b"
    :default (call-interactively 'switch-to-buffer)
    :C-u helm-projectile-switch-to-buffer))

(bind-keys :map helm-find-files-map
           ("C-h" . delete-backward-char)
           ("TAB" . helm-execute-persistent-action))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; lisp
;;;

(use-package paredit
  :defer t
  :config
  (bind-keys :map paredit-mode-map
             ("C-h" . paredit-backward-delete))

  (defun conditionally-enable-paredit-mode ()
    (if (eq this-command 'eval-expression)
        (paredit-mode 1)))
  (add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode))

(use-package eldoc
  :defer t
  :config
  (setq eldoc-idle-delay 0.1
        eldoc-minor-mode-string ""))

(use-package rainbow-delimiters
  :defer t)

(defun my/lisp-mode-defaults ()
  (paredit-mode 1)
  (rainbow-delimiters-mode 1)
  (eldoc-mode 1))

(defun my/lisp-mode-hook ()
  (my/lisp-mode-defaults))

(add-hook 'emacs-lisp-mode-hook 'my/lisp-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; clojure
;;;

(use-package clojure-mode
  :init
  (add-hook 'clojure-mode-hook #'yas-minor-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'my/lisp-mode-hook)
  (add-hook 'clojure-mode-hook 'highlight-symbol-mode)
  (add-hook 'clojure-mode-hook 'highlight-symbol-nav-mode))

(use-package cider
  :init
  ;; (add-hook 'cider-mode-hook #'clj-refactor-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'my/lisp-mode-hook)

  :diminish subword-mode

  :config
  (setq nrepl-log-messages t
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        cider-overlays-use-font-lock t)
  (cider-repl-toggle-pretty-printing)

  ;; Configure for figwheel
  ;; https://github.com/bhauman/lein-figwheel/wiki/Using-the-Figwheel-REPL-within-NRepl#integration-with-emacscider
  ;; !! ↓ Doesn't work properly... ↓ !!
  ;; (setq cider-cljs-lein-repl
  ;;       "(do (require 'figwheel-sidecar.repl-api)
  ;;            (figwheel-sidecar.repl-api/start-figwheel!)
  ;;            (figwheel-sidecar.repl-api/cljs-repl)")
  ;; https://github.com/Day8/re-frame-template#development-mode
  (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))"))

;; (use-package cider-eval-sexp-fu)

(use-package flycheck-clojure
  :init
  (eval-after-load 'flycheck '(flycheck-clojure-setup))
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-pos-tip
  :init
  (eval-after-load 'flycheck
  '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(defun my/clojure-mode-hook ()
  "Hook for clojure mode."
  (clj-refactor-mode 1)
  (yas-minor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c j"))

(use-package clj-refactor
  :config
  (add-hook 'clojure-mode-hook #'my/clojure-mode-hook))

(use-package cljr-helm
  :bind (("C-c r" . cljr-helm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ruby
;;;

(use-package slim-mode)
(use-package yaml-mode)
(use-package web-mode
  :config
  (setq-default indent-tabs-mode nil)
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-style-padding 2
        web-mode-code-indent-offset 2
        web-mode-script-padding 2
        web-mode-block-padding 2)
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))
;; (use-package ruby-block)
;; (use-package ruby-electric)

;; (defun my/ruby-mode-hook ()
;;   (ruby-electric-mode t)
;;   (when (require 'ruby-block nil t)
;;     (setq ruby-block-highlight-toggle t)
;;     (ruby-block-mode t)))

(use-package rubocop
  :config
  (add-hook 'rubocop-mode-hook 'rubocop-mode))

(use-package ruby-mode
  :init
  ;; (add-hook 'ruby-mode-hook #'my/ruby-mode-hook)
  (custom-set-variables
   '(ruby-insert-encoding-magic-comment nil))
  :config
  (add-hook 'ruby-mode-hook
          '(lambda ()
             (setq flycheck-checker 'ruby-rubocop)
             (setq-default flycheck-disabled-checkers '(ruby-rubylint))
             (flycheck-mode 1)))
  (add-hook 'ruby-mode-hook 'highlight-symbol-mode)
  (add-hook 'ruby-mode-hook 'highlight-symbol-nav-mode))

(use-package ruby-electric
  :pin melpa
  :config
  (add-hook 'ruby-mode-hook 'ruby-electric-mode))

(use-package ruby-refactor
  :pin melpa
  :config
  (add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; stylesheet
;;;

(use-package scss-mode
  :config
  (setq css-indent-offset 2
        scss-compile-at-save nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; markdown
;;;
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; javascript
;;;
(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.\\(js\\|jsx\\)$" . js2-mode))
  (custom-set-variables
   '(js2-basic-offset 2)
   '(js2-bounce-indent-p nil)
   '(jsx-indent-level 2))
  (setq js2-strict-missing-semi-warning nil
        js2-missing-semi-one-line-override t)
  :pin melpa-stable)

;;; json
(use-package json-mode
  :config
  (setq js-indent-level 2
        tab-width 2)
  :init
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
  (add-hook 'json-mode-hook #'company-mode))

(use-package babel
  :pin
  melpa-stable
  :init
  (add-to-list 'auto-mode-alist '("\\.babelrc$" . json-mode)))

(use-package vue-mode
 :pin
 melpa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; coffescript
;;;
(use-package coffee-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; python
;;;
(use-package company-jedi
  :config
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'my/python-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; IPython notebook(Jupyter notebook)
;;;
(use-package ein
  :pin melpa-stable
  :config
  (add-hook 'ein:connect-mode-hook 'ein:jedi-setup))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; rust
;;;
;; (use-package rust-mode)

(use-package racer
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (bind-keys :map rust-mode-map
             ("TAB" . company-indent-or-complete-common))
  (setq company-tooltip-align-annotations t
        racer-cmd "~/.cargo/bin/racer"
        racer-rust-src-path (getenv "RACER_RUST_SRC_PATH"))) ; Add `export RACER_RUST_SRC_PATH="~/works/github/rust/src"` to .zshenv!

(use-package cargo
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package flycheck-rust
  :pin melpa
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; wiki
;;;
(use-package mediawiki)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; fish
;;;
(use-package fish-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; php
;;;
(use-package php-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; docker
;;;
(use-package dockerfile-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PlantUML
;;;
(use-package plantuml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  ;; (setq plantuml-jar-path (getenv "PLANTUML_JAR_PATH"))
  (setq plantuml-jar-path "~/bin/plantuml.jar"))

(use-package flycheck-plantuml
  :pin melpa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CSV
;;;
(use-package csv-mode
  :pin elpa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; esa
;;;
(use-package esa
  :pin melpa
  :config
  (setq esa-token (getenv "ESA_ACCESS_TOKEN")
        esa-team-name (getenv "ESA_TEAM_NAME")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; apib-mode
;;;
(use-package apib-mode
  :pin melpa
  :config
  (add-to-list 'auto-mode-alist '("\\.apib\\'" . apib-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; go-mode
;;;
(use-package go-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; backup & auto saving
;;;
;; バックアップとオートセーブファイルを~/.emacs.d/backups/へ集める
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))

;; オートセーブファイル作成までの時間
(setq auto-save-timeout 15)
;; オートセーブファイル作成までのタイプ間隔
(setq auto-save-interval 60)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; after loading init.el
;;;

(message "init.el loaded!")

(add-hook 'after-init-hook
          (lambda ()
            (message "init time: %.3f sec"
                     (float-time (time-subtract after-init-time before-init-time)))))

(provide 'init)
;;; init.el ends here
