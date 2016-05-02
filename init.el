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
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

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
      inhibit-startup-message t)

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
  (setq whitespace-space-regexp "\\(\u3000+\\)")
  (global-whitespace-mode 1)
  (setq-default tab-width 4 indent-tabs-mode nil))

;;; cleanup whitespace before file save
(add-hook 'before-save-hook 'whitespace-cleanup)

;;; hl-line
(use-package hl-line
  :config
  (global-hl-line-mode)
  (set-face-background 'hl-line "#525252"))

;;; modeline
(setq display-time-string-forms
      '((format
         "%s/%s(%s) %s:%s" month day dayname 24-hours minutes))
      line-number-mode t
      column-number-mode t)
(display-time-mode 1)

;; http://flex.phys.tohoku.ac.jp/texi/eljman/eljman_142.html
(setq-default
 mode-line-format
 '(""
   mode-line-mule-info
   mode-line-modified
   " "
   mode-line-buffer-identification

   " | "
   (line-number-mode "L%l ")
   (column-number-mode "C%c ")
   (-3 . "%p")
   " | "
   mode-name
   minor-mode-alist "%n" mode-line-process
   " | "
   global-mode-string
   ))

(setq-default
 header-line-format
 '(""
   (:propertize (:eval (shorten-directory default-directory 30))
                face mode-line-folder-face)
   (:propertize "%b"
                face mode-line-filename-face)))

(defun shorten-directory (dir max-length)
  "Show up to `max-length` characters of a directory name `dir`."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))
