;;; init-misc.el --- misc utilities -*- lexical-binding: t; -*-
;;; Commentary:
;;; Miscellaneous utilities not tied to a specific language.
;;; Code:

;;; Symbol outline.
(use-package imenu-list
  :bind ("C-o" . imenu-list-smart-toggle))

;;; Quickly create throwaway files.
(use-package open-junk-file
  :bind ("C-x j" . open-junk-file)
  :custom
  (open-junk-file-format "~/works/junk/%Y/%m/%d-%H_%M_%S."))

;;; Quick preview from dired.
(use-package quick-preview
  :bind (("C-c q" . quick-preview-at-point)
         :map dired-mode-map
         ("Q" . quick-preview-at-point)))

;;; Edit browser text areas in Emacs.
(use-package edit-server
  :commands edit-server-start
  :init
  (when (display-graphic-p)
    (add-hook 'emacs-startup-hook #'edit-server-start))
  :config
  (setq edit-server-new-frame nil))

;;; Presentation mode for large text.
(use-package presentation
  :commands presentation-mode)

;;; MediaWiki editing.
(use-package mediawiki
  :commands mediawiki-open)

;;; esa.io integration.
(use-package esa
  :commands (esa esa-list)
  :config
  (setq esa-token (getenv "ESA_ACCESS_TOKEN")
        esa-team-name (getenv "ESA_TEAM_NAME")))

(provide 'init-misc)
;;; init-misc.el ends here
