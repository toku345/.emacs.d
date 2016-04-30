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

;; 入力されるキーシーケンスを置き換える
;; ?\C-?はDELのキーシーケンス
(keyboard-translate ?\C-h ?\C-?)
