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
  "Byte-compile all command-line file arguments.
Warnings fail the run via `byte-compile-error-on-warn'."
  (let ((files (my/byte-compile-batch--files))
        (failed nil)
        (byte-compile-error-on-warn t)
        (byte-compile-dest-file-function
         #'my/byte-compile-batch--dest-file))
    (add-to-list 'load-path (expand-file-name "lisp" default-directory))
    (dolist (file files)
      (message "Byte-compiling %s" file)
      (condition-case err
          ;; Judge loading by this file's own result, not the global flag;
          ;; an earlier failure must not skip loading later good modules.
          (let ((compiled (byte-compile-file file)))
            (unless compiled
              (setq failed t))
            (when (and compiled
                       (my/byte-compile-batch--load-after-compile-p file))
              (load (file-name-sans-extension (expand-file-name file))
                    nil t)))
        (error
         (setq failed t)
         (message "%s: %s" file (error-message-string err)))))
    (kill-emacs (if failed 1 0))))

(provide 'byte-compile-batch)
;;; byte-compile-batch.el ends here
