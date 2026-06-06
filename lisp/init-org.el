;;; init-org.el --- org-mode & blogging -*- lexical-binding: t; -*-
;;; Commentary:
;;; org-mode and HTML/Hugo export.
;;; Code:

(use-package htmlize
  :commands (htmlize-buffer htmlize-region))

(use-package org
  :ensure nil
  :bind ((:map global-map
               ("C-c c" . org-capture))
         (:map org-mode-map
               ("C-'" . undo-redo)))
  :custom
  (org-directory "~/works/org")
  (org-default-notes-file "notes.org")
  (org-startup-with-inline-images t)
  (org-html-doctype "html5")
  (org-html-html5-fancy t)
  (org-capture-templates
   '(("n" "Note" entry
      (file+headline "~/works/org/notes.org" "Notes")
      "* %?\nEntered on %U\n%i\n%a"))))

(use-package easy-hugo
  :bind ("C-c C-e" . easy-hugo)
  :custom
  (easy-hugo-basedir "~/works/toku345/toku345.com/")
  (easy-hugo-previewtime "300")
  (easy-hugo-url "https://blogs-toku345.firebaseapp.com")
  (easy-hugo-default-ext ".org")
  (easy-hugo-postdir "content/posts"))

(provide 'init-org)
;;; init-org.el ends here
