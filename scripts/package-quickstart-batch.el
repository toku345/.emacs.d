;;; package-quickstart-batch.el --- batch package quickstart refresh -*- lexical-binding: t; -*-
;;; Commentary:
;;; Refresh package quickstart in batch and fail on package activation errors.
;;; Code:

(require 'cl-lib)
(require 'package)

(defun my/package-quickstart-batch-refresh ()
  "Refresh `package-quickstart-file', failing on activation errors."
  (let ((activation-failures nil)
        (original-package-activate (symbol-function 'package-activate)))
    (cl-letf (((symbol-function 'package-activate)
               (lambda (package &optional force)
                 (condition-case err
                     (funcall original-package-activate package force)
                   (error
                    (push (format "%s: %s"
                                  package
                                  (error-message-string err))
                          activation-failures)
                    (signal (car err) (cdr err)))))))
      (package-quickstart-refresh))
    (when activation-failures
      (error "Package quickstart activation failures: %s"
             (mapconcat #'identity (nreverse activation-failures) "; ")))))

(provide 'package-quickstart-batch)
;;; package-quickstart-batch.el ends here
