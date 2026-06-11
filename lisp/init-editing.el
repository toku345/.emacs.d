;;; init-editing.el --- editing helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Editing helpers: structural editing, snippets, multiple cursors, undo, folds.
;;; Code:

;;; --- Shared Lisp editing defaults ---
(use-package paredit
  :defer t
  :bind (:map paredit-mode-map
              ("C-h" . paredit-backward-delete))
  :init
  ;; Register the hook in :init, not :config, so it is active before paredit
  ;; loads; paredit-mode is autoloaded and pulls paredit in on first use.
  (defun my/conditionally-enable-paredit-mode ()
    "Enable paredit only in the eval-expression minibuffer."
    (when (eq this-command 'eval-expression)
      (paredit-mode 1)))
  (add-hook 'minibuffer-setup-hook #'my/conditionally-enable-paredit-mode))

(use-package rainbow-delimiters
  :defer t)

(use-package eldoc
  :ensure nil
  :diminish
  :config
  (setq eldoc-idle-delay 0.1
        eldoc-minor-mode-string ""))

(defun my/lisp-mode-defaults ()
  "Apply shared defaults for Lisp-like modes."
  (paredit-mode 1)
  (rainbow-delimiters-mode 1)
  (eldoc-mode 1))

(defun my/lisp-mode-hook ()
  "Hook body for Lisp-like modes."
  (my/lisp-mode-defaults))

(add-hook 'emacs-lisp-mode-hook #'my/lisp-mode-hook)

;;; --- Snippets ---
(declare-function yas-reload-all "yasnippet")
(use-package yasnippet
  :diminish yas-minor-mode
  ;; Per-mode hooks instead of yas-global-mode keep yasnippet (and the
  ;; snippet directory scan) out of startup until the first file visit.
  :hook ((prog-mode text-mode) . yas-minor-mode)
  :config
  (yas-reload-all)
  :bind (:map yas-minor-mode-map
              ;; Leave TAB to corfu/indentation and avoid expansion conflicts.
              ("<tab>" . nil)
              ("TAB" . nil)
              ("C-i" . nil)))

;;; Snippet collection.
(use-package yasnippet-snippets
  :after yasnippet)

;;; --- Multiple cursors ---
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)))

;;; --- undo ---
;;; Replace undo-tree with built-in undo-redo plus vundo for visual history.
(use-package vundo
  :bind (("C-x u" . vundo)
         ("C-'"   . undo-redo)))

;;; --- Folding ---
(declare-function treesit-fold-ready-p "treesit-fold")
(declare-function treesit-fold-toggle "treesit-fold")
(declare-function treesit-fold-usable-mode-p "treesit-fold")

(defun my/treesit-fold-toggle ()
  "Toggle a tree-sitter fold when the current buffer supports it.
Loads treesit-fold on first use; its :config enables the global mode."
  (interactive)
  (if (and (require 'treesit-fold nil t)
           (treesit-fold-ready-p)
           (treesit-fold-usable-mode-p))
      (treesit-fold-toggle)
    (message "No tree-sitter folding available in this buffer")))

(use-package treesit-fold
  :defer t                          ; Loaded by my/treesit-fold-toggle above.
  :bind (:map global-map
              ("M-RET" . my/treesit-fold-toggle))
  :config
  (setq treesit-fold-line-count-show t)
  (global-treesit-fold-mode 1))

;;; --- Spell checking ---
;;; jinx compiles a native module against the enchant C library on first
;;; load, so enable it only when the development headers are present
;;; (libenchant-2-dev on Debian/Ubuntu, enchant via Homebrew on macOS).
(use-package jinx
  :if (ignore-errors
        (eq 0 (call-process "pkg-config" nil nil nil "--exists" "enchant-2")))
  :hook (text-mode . jinx-mode)         ; org and markdown derive from it.
  :bind ("M-$" . jinx-correct))         ; Replaces the ispell-word default.

(provide 'init-editing)
;;; init-editing.el ends here
