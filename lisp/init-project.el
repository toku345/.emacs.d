;;; init-project.el --- project & search -*- lexical-binding: t; -*-
;;; Commentary:
;;; Project management uses built-in project.el, replacing projectile.
;;; Search-related setup also lives here.
;;; Code:

;;; Built-in project.el with the old projectile keys, C-c p and s-p.
(use-package project
  :ensure nil
  :bind-keymap (("C-c p" . project-prefix-map)
                ("s-p"   . project-prefix-map)))

;;; Edit grep buffers directly; this works well with consult/embark export.
(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

;;; Definition-jump fallback registered as an xref backend.
(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :config
  (setq dumb-jump-prefer-searcher 'rg))

(provide 'init-project)
;;; init-project.el ends here
