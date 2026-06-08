;;; checkdoc-batch.el --- batch checkdoc runner -*- lexical-binding: t; -*-
;;; Commentary:
;;; Run checkdoc over files passed after -- and exit nonzero on warnings.
;;; Code:

(require 'checkdoc)

(defun my/checkdoc-batch--files ()
  "Return file arguments passed to this batch process."
  (let (files)
    (dolist (arg command-line-args-left)
      (unless (string-prefix-p "-" arg)
        (push arg files)))
    (nreverse files)))

(defun my/checkdoc-batch-run ()
  "Run `checkdoc-file' over all command-line file arguments."
  (let ((failed nil)
        (checkdoc-spellcheck-documentation-flag nil)
        (checkdoc-autofix-flag 'never))
    (dolist (file (my/checkdoc-batch--files))
      (message "Checking documentation in %s" file)
      (let ((checkdoc-pending-errors nil))
        (condition-case err
            (progn
              (checkdoc-file file)
              (when checkdoc-pending-errors
                (setq failed t)))
          (error
           (setq failed t)
           (message "%s: %s" file (error-message-string err))))))
    (kill-emacs (if failed 1 0))))

(provide 'checkdoc-batch)
;;; checkdoc-batch.el ends here
