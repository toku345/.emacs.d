;;; init-ui.el --- appearance & modeline -*- lexical-binding: t; -*-
;;; Commentary:
;;; Appearance: theme, modeline, whitespace display, line highlights, and more.
;;; Code:

;;; Theme. Keep zenburn for now; switch to built-in modus-themes with
;;; (load-theme 'modus-vivendi t) if desired.
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t)
  ;; Preserve colors from the old configuration.
  (with-eval-after-load 'hl-line
    (set-face-background 'hl-line "#525252"))
  (set-face-background 'region "dark green"))

;;; Modeline.
(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t
        sml/theme 'respectful
        sml/shorten-directory -1)
  :config
  (sml/setup)
  (column-number-mode 1)
  (line-number-mode 1))

(display-time-mode 1)

;;; Keep minor-mode lighters concise.
(use-package diminish)

;;; Whitespace visualization, preserving the old display mappings.
(use-package whitespace
  :ensure nil
  :diminish global-whitespace-mode
  :config
  (setq whitespace-style '(face trailing tabs spaces empty space-mark tab-mark)
        whitespace-display-mappings
        '((space-mark ?　 [?□])
          (tab-mark ?\t [?» ?\t] [?\\ ?\t]))
        whitespace-space-regexp "\\(\x3000+\\|^ +\\| +$\\)")
  (global-whitespace-mode 1))

;;; Clean trailing whitespace before save.
(add-hook 'before-save-hook #'whitespace-cleanup)

;;; Highlight the current line using global-hl-line-mode.
(use-package hl-line
  :ensure nil
  :config
  (global-hl-line-mode 1))

;;; Symbol highlighting and navigation, replacing highlight-symbol.
(use-package symbol-overlay
  :diminish
  :hook (prog-mode . symbol-overlay-mode)
  :bind (("M-s h" . symbol-overlay-put)
         ("M-s n" . symbol-overlay-jump-next)
         ("M-s p" . symbol-overlay-jump-prev)))

(provide 'init-ui)
;;; init-ui.el ends here
