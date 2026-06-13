;;; package-quickstart-batch.el --- batch package quickstart refresh -*- lexical-binding: t; -*-
;;; Commentary:
;;; Refresh package quickstart in batch and fail on package activation errors.
;;; Code:

(require 'package)

(defun my/package-quickstart-batch-refresh ()
  "Refresh `package-quickstart-file', failing on activation errors."
  (my/package-quickstart-refresh-with-activation-check
   #'package-quickstart-refresh))

(defun my/package-quickstart-batch-check-configured-file ()
  "Verify the configured quickstart file is loadable when it exists."
  (when (file-exists-p package-quickstart-file)
    (load package-quickstart-file nil nil)))

(defun my/package-quickstart-batch-check ()
  "Verify configured and temporary package quickstart files."
  (my/package-quickstart-batch-check-configured-file)
  (let ((package-quickstart-file
         (make-temp-file "package-quickstart-check" nil ".el")))
    (unwind-protect
        (progn
          (my/package-quickstart-batch-refresh)
          (unless (file-exists-p package-quickstart-file)
            (error "Package quickstart file was not generated"))
          (load package-quickstart-file nil t))
      (dolist (file (list package-quickstart-file
                          (concat package-quickstart-file "c")))
        (when (file-exists-p file)
          (delete-file file))))))

(provide 'package-quickstart-batch)
;;; package-quickstart-batch.el ends here
