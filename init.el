;; -*- mode: Emacs-Lisp ; Coding: utf-8 -*-
(set-language-environment "UTF-8")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;;;;;;;
;;;
;;; basic configurations
;;;

;;; enable use-package
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-always-pin "melpa-stable")
(setq use-package-verbose t)

;;; load my preferred theme
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

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
	     ("C-x ?" . help-command)))

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
