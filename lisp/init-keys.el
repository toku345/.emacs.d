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
           ("C-c l"   . toggle-truncate-lines)
           ("C-t"     . other-window)
           ("C-x ?"   . help-command)
           ("C-x SPC" . cua-rectangle-mark-mode)
           ("M-n"     . (lambda () (interactive) (scroll-up 1)))
           ("M-p"     . (lambda () (interactive) (scroll-down 1)))
           ("M-["     . switch-to-prev-buffer)
           ("M-]"     . switch-to-next-buffer)
           ("C-c C-p" . my/move-line-up)
           ("C-c C-n" . my/move-line-down))

(provide 'init-keys)
;;; init-keys.el ends here
