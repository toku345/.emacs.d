;;; init-editing.el --- editing helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Editing helpers: structural editing, snippets, multiple cursors, undo, folds.
;;; Code:

;;; --- Shared Lisp editing defaults ---
(use-package paredit
  :defer t
  :bind (:map paredit-mode-map
              ("C-h" . paredit-backward-delete))
  :config
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
(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1)
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
  "Toggle a tree-sitter fold when the current buffer supports it."
  (interactive)
  (if (and (fboundp 'treesit-fold-ready-p)
           (fboundp 'treesit-fold-usable-mode-p)
           (fboundp 'treesit-fold-toggle)
           (treesit-fold-ready-p)
           (treesit-fold-usable-mode-p))
      (treesit-fold-toggle)
    (message "No tree-sitter folding available in this buffer")))

(use-package treesit-fold
  :demand t
  :bind (:map global-map
              ("M-RET" . my/treesit-fold-toggle))
  :config
  (setq treesit-fold-line-count-show t)
  (global-treesit-fold-mode 1))

(provide 'init-editing)
;;; init-editing.el ends here
