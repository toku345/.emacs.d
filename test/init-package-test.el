;;; init-package-test.el --- tests for init-package -*- lexical-binding: t; -*-
;;; Commentary:
;;; ERT tests for package startup initialization behavior.  Run after loading
;;; the full configuration:
;;;   emacs -Q --batch -l early-init.el -l init.el \
;;;     -l scripts/package-quickstart-batch.el -l test/init-package-test.el \
;;;     -f ert-run-tests-batch-and-exit
;;; Code:

(require 'cl-lib)
(require 'ert)

(declare-function my/package-quickstart-batch-refresh "package-quickstart-batch")

(ert-deftest init-package-test-quickstart-keeps-package-metadata ()
  "Quickstart activation initializes package.el without reactivating packages."
  (let ((package--initialized nil)
        (package--activated t)
        (package-initialize-argument :unset))
    (cl-letf (((symbol-function 'package-initialize)
               (lambda (&optional no-activate)
                 (setq package-initialize-argument no-activate
                       package--initialized t))))
      (my/package-initialize)
      (should (eq package-initialize-argument 'no-activate))
      (should package--initialized))))

(ert-deftest init-package-test-quickstart-refresh-fails-on-activation-error ()
  "Batch quickstart refresh fails when refresh hides activation errors."
  (let ((package-quickstart-called nil))
    (cl-letf (((symbol-function 'package-activate)
               (lambda (&rest _args)
                 (error "Simulated activation failure")))
              ((symbol-function 'package-quickstart-refresh)
               (lambda ()
                 (setq package-quickstart-called t)
                 (condition-case err
                     (package-activate 'broken-package)
                   (error
                    (message "%s" (error-message-string err)))))))
      (should-error (my/package-quickstart-batch-refresh))
      (should package-quickstart-called))))

(provide 'init-package-test)
;;; init-package-test.el ends here
