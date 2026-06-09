;;; init-core.el --- basic editor defaults -*- lexical-binding: t; -*-
;;; Commentary:
;;; Core editor defaults and behavior that do not depend on specific packages.
;;; Code:

;;; Import shell PATH for GUI and daemon sessions.
;;; GNU Emacs for NS also uses window-system 'ns; daemon sessions are included.
(use-package exec-path-from-shell
  :if (or (memq window-system '(mac ns x)) (daemonp))
  :config
  (exec-path-from-shell-initialize))

;;; Allow y/n answers without using the older defalias approach.
(setq use-short-answers t)

;;; General defaults ported from the old init.el.
(setq visible-bell t
      ring-bell-function 'ignore
      echo-keystrokes 0.1
      create-lockfiles nil
      truncate-partial-width-windows nil
      kill-whole-line t                 ; Let C-k delete the newline too.
      completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      frame-title-format "%f"
      uniquify-buffer-name-style 'post-forward)

;;; Indentation: spaces instead of tabs, width 2.
(setq-default tab-width 2
              indent-tabs-mode nil)

;;; macOS GUI modifier keys: make Command act as Meta and Option as Super.
(when (eq system-type 'darwin)
  (setq ns-command-modifier 'meta
        ns-option-modifier 'super
        mac-command-modifier 'meta
        mac-option-modifier 'super))

;;; Buffer boundary display.
(set-default 'indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'right)

;;; Replace the selected region when typing.
(delete-selection-mode 1)

;;; Enable automatic pairing globally.
(electric-pair-mode 1)

;;; Highlight matching parentheses.
(use-package paren
  :ensure nil
  :init
  (setq show-paren-style 'parenthesis)
  :config
  (show-paren-mode 1))

;;; Treat CamelCase parts as subwords.
(use-package subword
  :ensure nil
  :config
  (global-subword-mode 1))

;;; Automatically reload changed files.
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

;;; Make scripts executable on save when they start with #!.
(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

;;; Start the emacsclient server for GUI and daemon sessions, avoiding duplicates.
(when (or (display-graphic-p) (daemonp))
  (require 'server)
  (unless (server-running-p)
    (server-start)))

;;; dired
(use-package dired
  :ensure nil
  :config
  (setq dired-dwim-target t            ; Guess copy/move target in two-pane dired.
        dired-recursive-copies 'always
        dired-isearch-filenames t
        dired-listing-switches "-alh")
  ;; macOS ls lacks some GNU options, so prefer coreutils gls.
  ;; Support both Apple Silicon /opt/homebrew and Intel /usr/local.
  (when-let ((gls (executable-find "gls")))
    (setq insert-directory-program gls
          dired-use-ls-dired t)))

;;; Disable backup files and delete transient auto-save files after saving.
(setq backup-inhibited t
      delete-auto-save-files t
      auto-save-timeout 15
      auto-save-interval 60)

;;; Recent files and minibuffer history.
(use-package recentf
  :ensure nil
  :init
  (setq recentf-max-saved-items 300
        recentf-auto-cleanup 'never)
  :config
  (recentf-mode 1))

(use-package savehist
  :ensure nil
  :config
  (savehist-mode 1))

(use-package saveplace
  :ensure nil
  :config
  (save-place-mode 1))

;;; EditorConfig for project-local style consistency.
(use-package editorconfig
  :config
  (editorconfig-mode 1))

(provide 'init-core)
;;; init-core.el ends here
