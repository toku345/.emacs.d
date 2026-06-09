;;; byte-compile-batch.el --- batch byte-compile runner -*- lexical-binding: t; -*-
;;; Commentary:
;;; Compile repository Emacs Lisp files in a batch process.
;;; Code:

(require 'bytecomp)

(defvar my/byte-compile-batch-output-directory
  (make-temp-file "emacs-byte-compile-" t)
  "Directory where batch byte compilation writes temporary .elc files.")

(defun my/byte-compile-batch--files ()
  "Return file arguments passed to this batch process."
  (let (files)
    (dolist (arg command-line-args-left)
      (unless (string-prefix-p "-" arg)
        (push arg files)))
    (nreverse files)))

(defun my/byte-compile-batch--dest-file (source-file)
  "Return the temporary compiled output path for SOURCE-FILE."
  (let* ((relative (file-relative-name (expand-file-name source-file)
                                       default-directory))
         (dest (expand-file-name
                (concat (file-name-sans-extension relative) ".elc")
                my/byte-compile-batch-output-directory)))
    (make-directory (file-name-directory dest) t)
    dest))

(defun my/byte-compile-batch--load-after-compile-p (file)
  "Return non-nil when FILE should be loaded after byte compilation."
  (let ((relative (file-relative-name (expand-file-name file)
                                      default-directory)))
    (string-prefix-p "lisp/init-" relative)))

(defun my/byte-compile-batch-run ()
  "Byte-compile all command-line file arguments."
  (let ((files (my/byte-compile-batch--files))
        (failed nil)
        (byte-compile-dest-file-function
         #'my/byte-compile-batch--dest-file))
    (add-to-list 'load-path (expand-file-name "lisp" default-directory))
    (dolist (file files)
      (message "Byte-compiling %s" file)
      (condition-case err
          (progn
            (unless (byte-compile-file file)
              (setq failed t))
            (when (and (not failed)
                       (my/byte-compile-batch--load-after-compile-p file))
              (load (file-name-sans-extension (expand-file-name file))
                    nil t)))
        (error
         (setq failed t)
         (message "%s: %s" file (error-message-string err)))))
    (kill-emacs (if failed 1 0))))

(provide 'byte-compile-batch)
;;; byte-compile-batch.el ends here
