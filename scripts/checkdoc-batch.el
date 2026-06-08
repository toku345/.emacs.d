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

(defun my/checkdoc-batch--diagnostics (start)
  "Return checkdoc diagnostics added after START, or nil when clean."
  (with-current-buffer checkdoc-diagnostic-buffer
    (save-excursion
      (goto-char start)
      (when (re-search-forward "^.+:[0-9]+: " nil t)
        (buffer-substring-no-properties start (point-max))))))

(defun my/checkdoc-batch--check-file (file)
  "Run checkdoc over FILE and return diagnostics, or nil when clean."
  (with-current-buffer (find-file-noselect file)
    (let ((checkdoc-diagnostic-buffer "*checkdoc-batch*")
          (start nil))
      (with-current-buffer (get-buffer-create checkdoc-diagnostic-buffer)
        (let ((inhibit-read-only t))
          (erase-buffer))
        (setq start (point-min)))
      (checkdoc-current-buffer t)
      (my/checkdoc-batch--diagnostics start))))

(defun my/checkdoc-batch-run ()
  "Run `checkdoc-file' over all command-line file arguments."
  (let ((failed nil)
        (checkdoc-spellcheck-documentation-flag nil)
        (checkdoc-autofix-flag 'never))
    (dolist (file (my/checkdoc-batch--files))
      (message "Checking documentation in %s" file)
      (condition-case err
          (let ((diagnostics (my/checkdoc-batch--check-file file)))
            (when diagnostics
              (setq failed t)
              (princ diagnostics)))
        (error
         (setq failed t)
         (message "%s: %s" file (error-message-string err)))))
    (kill-emacs (if failed 1 0))))

(provide 'checkdoc-batch)
;;; checkdoc-batch.el ends here
