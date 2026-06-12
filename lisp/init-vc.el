;;; init-vc.el --- version control -*- lexical-binding: t; -*-
;;; Commentary:
;;; Git integration with magit and diff-hl, replacing git-gutter.
;;; Code:

(use-package magit
  :bind (("C-x M-g" . magit-dispatch)   ; Replace removed magit-dispatch-popup.
         ("C-c m"   . magit-status)
         ("C-c b"   . magit-blame))
  :config
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-todos
  :after magit
  :config
  (magit-todos-mode 1))

;;; Fringe diff display, replacing git-gutter and integrating with magit.
(use-package diff-hl
  :hook (dired-mode . diff-hl-dired-mode)
  :init
  ;; Covers every file-visiting buffer, so no per-mode diff-hl-mode hooks.
  (global-diff-hl-mode 1)
  :config
  ;; Update diffs live while editing.
  (diff-hl-flydiff-mode 1)
  ;; TTY has no fringe, so display diffs in the margin.
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1))
  ;; Refresh diff display around magit operations.
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  ;; Hunk operations. Use C-c g to avoid C-x p/n conflicts with project.el/narrow.
  :bind (("C-c g n" . diff-hl-next-hunk)
         ("C-c g p" . diff-hl-previous-hunk)
         ("C-c g r" . diff-hl-revert-hunk)
         ("C-c g s" . diff-hl-show-hunk)))

(provide 'init-vc)
;;; init-vc.el ends here
