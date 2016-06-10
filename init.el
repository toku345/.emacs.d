;; -*- mode: Emacs-Lisp ; Coding: utf-8 -*-
(set-language-environment "UTF-8")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; setup package.el
;;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(when (not (require 'use-package nil t))
  (package-install 'use-package))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; basic configurations
;;;

;;; enable use-package
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-always-pin "melpa-stable")
(setq use-package-verbose t)

;;; setup exec-path
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;; global keymap
(use-package bind-key
  :config
  (bind-keys :map global-map
             ("C-h" . delete-backward-char)
             ("C-m" . newline-and-indent)
             ("C-c l" . toggle-truncate-lines)
             ("C-t" . other-window)
             ("C-x ?" . help-command)
             ("C-x SPC" . cua-rectangle-mark-mode)))

;;; delete selection mode
(delete-selection-mode t)

;;; magit
(use-package magit
  :bind (("C-x M-g" . magit-dispatch-popup)
         ("C-c C-m" . magit-status)
         ("C-c C-b" . magit-blame))
  :pin melpa-stable)

;;; elscreen
(use-package elscreen
  :pin melpa
  :init
  (elscreen-start))

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
  (setq company-idle-delay 0.3
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
             ("C-o" . se/make-summary-buffer)))

;;; undo-tree
(use-package undo-tree
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
      dired-isearch-filenames t)

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
(use-package paradox)

;;; quelpa
(use-package quelpa
  :init
  (package-initialize)
  (if (require 'quelpa nil t)
      (quelpa-self-upgrade)
    (with-temp-buffer
      (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
      (eval-buffer))))

;;; executable-make-buffer-file-executable-if-script-p
;; ファイル名が #! から始まる場合、+xをつけて保存する
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;; global-auto-revert-mode
(global-auto-revert-mode 1)

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
  (setq-default tab-width 4 indent-tabs-mode nil))

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

;; Need install cmigemo
;; `$ brew install cmigemo`
(use-package migemo
  :config
  (setq migemo-command "cmigemo"
        migemo-options '("-q" "--emacs")
        migemo-dictionary "/usr/local/Cellar/cmigemo/20110227/share/migemo/utf-8/migemo-dict"
        migemo-user-dictionary nil
        migemo-regex-dictionary nil
        migemo-coding-system 'utf-8-unix)
  (migemo-init))

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

(use-package avy-migemo
  :config
  (avy-migemo-mode 1)
  (require 'avy-migemo-e.g.swiper))

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
  :bind ("C-c b" . helm-descbinds)
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
  (add-hook 'clojure-mode-hook #'my/lisp-mode-hook))

(use-package cider
  :init
  (add-hook 'cider-mode-hook #'clj-refactor-mode)
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
  (cider-repl-toggle-pretty-printing))

(use-package cider-eval-sexp-fu)

(use-package clj-refactor
  :diminish clj-refactor-mode
  :config (cljr-add-keybindings-with-prefix "C-c j"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ruby
;;;

(use-package slim-mode)
(use-package yaml-mode)
(use-package web-mode)
(use-package ruby-block)
(use-package ruby-electric)

(defun my/ruby-mode-hook ()
  (ruby-electric-mode t)
  (when (require 'ruby-block nil t)
    (setq ruby-block-highlight-toggle t)
    (ruby-block-mode t)))

(use-package ruby-mode
  :init
  (add-hook 'ruby-mode-hook #'my/ruby-mode-hook)
  (custom-set-variables
   '(ruby-insert-encoding-magic-comment nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; stylesheet
;;;

(use-package scss-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; markdown
;;;
(use-package markdown-mode)

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
  :pin melpa-stable)

;;; json
(use-package json-mode
  :config
  (setq js-indent-level 2
        tab-width 2)
  :init
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
  (add-hook 'json-mode-hook #'company-mode))

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
