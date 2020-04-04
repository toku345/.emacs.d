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
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(when (not (require 'use-package nil t))
  (package-install 'use-package))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; basic configurations
;;;

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)


;;; enable use-package
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-always-pin "melpa-stable")
(setq use-package-verbose t)

(use-package use-package-ensure-system-package
  :pin melpa
  :ensure t)

;;; setup exec-path
;;; https://github.com/purcell/exec-path-from-shell/blob/master/exec-path-from-shell.el#L88-L89
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

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
             ("C-c t" . toggle-truncate-lines)
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

;;; electric-pair-mode: set globally
(electric-pair-mode t)

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
  :pin melpa
  :ensure t
  ;; :init
  ;; (global-flycheck-mode) ; disabled
  )

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
             ;; ("C-o" . yas/expand)
             ))

;;; company-mode
(use-package company
  :pin melpa
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-global-modes '(not magit-status-mode))
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
  (projectile-mode +1)
  :config
  (bind-keys :map projectile-mode-map
             ("s-p" . projectile-command-map)
             ("C-c p" . projectile-command-map)))

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

;;; undo-tree
(use-package undo-tree
  :pin elpa
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
  :bind (("C-x C-g" . git-gutter-mode)
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

;;; imenu-list
(use-package imenu-list
  :bind
  ("C-o" . imenu-list-smart-toggle))

;;; open-junk-file
(use-package open-junk-file
  :pin melpa
  :bind
  ("C-x j" . open-junk-file)
  :custom
  (open-junk-file-format "~/works/junk/%Y/%m/%d-%H_%M_%S."))

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

;; smart-mode-line
;;   https://qiita.com/blue0513/items/99476f4ae51f17600636
(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'respectful)
  (setq sml/shorten-directory -1) ; Full path
  (sml/setup)
  :config
  (column-number-mode t)
  (line-number-mode) t)
;; diminish
(use-package diminish
  :init
  (eval-after-load "company" '(diminish 'company-mode "Comp"))  ;; Minor Mode名を変更
  (eval-after-load "ivy" '(diminish 'ivy-mode))) ;; Minor mode 非表示

; Count the number of lines / chars in the region: => M-= (count-words-region)

(use-package dumb-jump
  :pin melpa
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g q" . dumb-jump-quick-look))
  :custom
  (dumb-jump-selector 'helm)
  :ensure)

(use-package presentation
  :pin melpa)


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

;;; wgrep
(use-package wgrep)

;;; quick-preview
(use-package quick-preview
  :pin melpa
  :bind ("C-c q" . 'quick-preview-at-point)
  :config (define-key dired-mode-map (kbd "Q") 'quick-preview-at-point))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; helm
;;;

(use-package helm-ag
  :config
  (setq helm-ag-base-command "ag --nocolor --nogrou --ignore-case"
        helm-ag-thing-at-point 'symbol)
  :bind (("C-c s" . helm-ag)
         ("C-c f" . helm-ag-this-file)))

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


(use-package paredit
  :defer t
  :config
  (bind-keys :map paredit-mode-map
             ("C-h" . paredit-backward-delete))

  (defun conditionally-enable-paredit-mode ()
    (if (eq this-command 'eval-expression)
        (paredit-mode 1)))
  (add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode))

(use-package smartparens)

;;; EditorConfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; lisp
;;;

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
  :pin melpa
  :init
  (add-hook 'clojure-mode-hook #'yas-minor-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'my/lisp-mode-hook)
  (add-hook 'clojure-mode-hook 'highlight-symbol-mode)
  (add-hook 'clojure-mode-hook 'highlight-symbol-nav-mode)
  :config
  (define-clojure-indent
    ;; Settings for Compojure
    ;; https://github.com/weavejester/compojure/wiki/Emacs-indentation
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)))

(use-package cider
  :pin melpa
  :init
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'my/lisp-mode-hook)

  :diminish subword-mode

  :config
  (setq nrepl-log-messages t
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t
        cider-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        cider-overlays-use-font-lock t
        cider-repl-toggle-pretty-printing t)
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

;; Deadly slow....
;; (use-package flycheck-clojure
;;   :init
;;   (eval-after-load 'flycheck '(flycheck-clojure-setup))
;;   (add-hook 'after-init-hook #'global-flycheck-mode))
;;
;; (use-package flycheck-pos-tip
;;   :init
;;   (eval-after-load 'flycheck
;;   '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(defun my/clojure-mode-hook ()
  "Hook for clojure mode."
  (yas-minor-mode 1)
  ;; (setq-default flycheck-disabled-checkers '(clojure-cider-eastwood))
  )

(use-package helm-cider
  :init
  (helm-cider-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; scheme
;;;
(use-package geiser
  :init
  (add-hook 'geiser-mode-hook #'my/lisp-mode-hook)
  (add-hook 'geiser-repl-mode-hook #'my/lisp-mode-hook)
  :config
  (setq geiser-active-implementations '(racket)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; common lisp
;;;

(if (file-exists-p "~/.roswell/helper.el")
    (progn
      (load (expand-file-name "~/.roswell/helper.el"))
      (add-hook 'sly-mode-hook #'my/lisp-mode-hook)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; yaml
;;;

(use-package yaml-mode
  :config
  (add-hook 'yaml-mode-hook #'display-line-numbers-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; prettier
;;;

(use-package prettier-js
  :pin melpa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; web
;;;

(use-package web-mode
  :config
  (setq-default indent-tabs-mode nil)
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-style-padding 2
        web-mode-code-indent-offset 2
        web-mode-script-padding 2
        web-mode-block-padding 2
        web-mode-enable-engine-detection t)
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))
  (add-hook 'web-mode-hook #'prettier-js-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ruby
;;;

(use-package slim-mode
  :config
  (add-hook 'slim-mode-hook #'display-line-numbers-mode))

(use-package rubocop
  :config
  (add-hook 'rubocop-mode-hook 'rubocop-mode))

(use-package ruby-mode
  :init
  (custom-set-variables
   '(ruby-insert-encoding-magic-comment nil))
  :config
  (add-hook 'ruby-mode-hook
          '(lambda ()
             (setq flycheck-checker 'ruby-rubocop)
             (setq-default flycheck-disabled-checkers '(ruby-rubylint))
             (display-line-numbers-mode t)))
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
        scss-compile-at-save nil)
  (add-hook 'css-mode-hook #'prettier-js-mode))

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

(eval-after-load 'js
  (add-hook 'js-mode-hook #'prettier-js-mode))


;;; json
(use-package json-mode
  :config
  (setq js-indent-level 2
        tab-width 2)
  (setq-default flycheck-disabled-checkers '(json-python-json))
  :init
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
  (add-hook 'json-mode-hook #'company-mode))

;;; JSON Reformat
;;; https://qiita.com/saku/items/d97e930ffc9ca39ac976
(defun jq-format (beg end)
  "Reformat by jq"
  (interactive "r")
  (shell-command-on-region beg end "jq ." nil t))

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
(use-package coffee-mode
  :config
  (add-hook 'coffee-mode-hook #'display-line-numbers-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; typescript
;;;
(use-package typescript-mode
  :pin melpa
  :config
  (add-hook 'typescript-mode-hook #'prettier-js-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Angular
;;;
(use-package ng2-mode
  :pin melpa
  :config
  (with-eval-after-load 'typescript-mode (add-hook 'typescript-mode-hook #'lsp))
  (setq typescript-indent-level 2
        sgml-basic-offse 2)
  (add-hook 'ng2-ts-mode #'prettier-js-mode)
  (add-hook 'ng2-html-mode #'prettier-js-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; elm
;;;
(use-package elm-mode
  :pin melpa
  :init
  (setq elm-format-on-save t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; python
;;;

(use-package python-mode
  :config
  (add-hook 'python-mode-hook
            (lambda () (add-hook 'before-save-hook #'lsp-format-buffer nil t))))

(use-package poetry
  :pin melpa
  :ensure t)

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
(use-package rust-mode
  :pin melpa
  :init
  (add-hook 'rust-mode-hook #'flycheck-mode)
  :config
  (setq rust-format-on-save t))

(use-package flycheck-rust
  :pin melpa
  :after
  (flycheck rust-mode)
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package cargo
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; lsp
;;;
(use-package lsp-mode
  :pin melpa
  :commands lsp
  :init
  (add-hook 'prog-mode-hook #'lsp)
  :config
  (setq lsp-log-io t
        lsp-print-performance t
        lsp-inhibit-message t
        lsp-auto-guess-root t
        lsp-prefer-flymake t
        lsp-enable-completion-at-point t
        lsp-report-if-no-buffer t))

(use-package lsp-ui
  :pin melpa
  :commands lsp-ui-mode)


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
;;; SQL
;;;
(use-package sql-indent
  :pin elpa
  :config
  (eval-after-load "sql"
    '(load-library "sql-indent")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; java
;;;
(use-package meghanada
  :config
  (setq meghanada-java-path "java"
        meghanada-maven-path "mvn"))

(add-hook 'java-mode-hook
          (lambda ()
            (meghanada-mode t)
            (flycheck-mode +1)
            (setq c-basic-offset 2)
            ;; use code format
            (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))

;;; groovy
(use-package groovy-mode)

;;; gradle
(use-package gradle-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; scala
;;;
;;; Ensime -> Metals : https://scalameta.org/metals/docs/editors/emacs.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; kotlin
;;;
(use-package kotlin-mode
  :pin melpa)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ABC
;;;
(use-package abc-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; F#
;;;
(use-package fsharp-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Erlang
;;;
(use-package erlang
  :pin melpa)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dart
;;;
(use-package dart-mode
  :pin melpa
  :hook (dart-mode . lsp)
  :after (lsp projectile)
  :ensure-system-package (dart_language_server . "pub global activate dart_language_server")
  :custom
  (dart-format-on-save t)
  (dart-sdk-path "~/flutter/bin/cache/dart-sdk/")
  :config
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Flutter
;;;
(use-package flutter
  :pin melpa
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-on-hot-reload))
  :custom
  (flutter-sdk-path "~/flutter/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; org-mode
;;;
(use-package org
  :after htmlize
  :bind
  (:map global-map
        ("C-c c" . #'org-capture)
        ("C-c l" . #'org-store-link))
  (:map org-mode-map
        ("C-'" . #'redo)) ; org-agenda-sycle
  :custom
  (org-directory "~/works/org")
  (org-default-notes-file "notes.org")
  (org-startup-with-inline-images t)
  (org-html-doctype "html5")
  (org-html-html5-fancy t)
  (org-capture-templates
   '(("n" "Note" entry
      (file+headline "~/works/org/notes.org" "Notes")
      "* %?\nEntered on %U\n%i\n%a"))))

(use-package htmlize)

(use-package easy-hugo
  :custom
  (easy-hugo-basedir "~/works/toku345/toku345.com/")
  (easy-hugo-previewtime "300")
  (easy-hugo-url "https://blogs-toku345.firebaseapp.com")
  (easy-hugo-default-ext ".org")
  (easy-hugo-postdir "content/posts")
  :bind ("C-c C-e" . easy-hugo))


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
