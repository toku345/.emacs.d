;;; init-keys.el --- global key bindings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Global key bindings.  Package-specific bindings live in their modules.
;;; Preserve the feel of the old init.el bindings.
;;; Code:

(require 'bind-key)

;;; Move the current line up or down.
;;; http://emacsredux.com/blog/2013/04/02/move-current-line-up-or-down/
(defun my/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun my/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;;; Global keymap, following the old configuration.
(bind-keys :map global-map
           ("C-h"     . delete-backward-char)  ; Help is moved to C-x ?.
           ("C-m"     . newline-and-indent)
           ("C-c t"   . toggle-truncate-lines) ; C-c l belongs to org-store-link.
           ("C-t"     . other-window)
           ("C-x ?"   . help-command)
           ("C-x SPC" . cua-rectangle-mark-mode)
           ;; The old M-n/M-p line-scroll bindings are retired in all modes,
           ;; not only where flymake-mode-map shadows them for diagnostics
           ;; navigation (init-lsp.el).
           ("M-["     . switch-to-prev-buffer)
           ("M-]"     . switch-to-next-buffer)
           ;; C-c C-<letter> is major-mode territory and was shadowed in
           ;; org, markdown, python, etc. org rebinds M-<up>/M-<down> to
           ;; org-metaup/org-metadown, which move lines/elements the same way.
           ("M-<up>"   . my/move-line-up)
           ("M-<down>" . my/move-line-down))

(provide 'init-keys)
;;; init-keys.el ends here
