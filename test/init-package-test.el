;;; init-package-test.el --- tests for init-package -*- lexical-binding: t; -*-
;;; Commentary:
;;; ERT tests for package startup initialization behavior.  Run after loading
;;; the full configuration:
;;;   emacs -Q --batch -l early-init.el -l init.el \
;;;     -l test/init-package-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'cl-lib)
(require 'ert)

(ert-deftest init-package-test-quickstart-keeps-package-metadata ()
  "Quickstart activation still loads package descriptor metadata."
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

(provide 'init-package-test)
;;; init-package-test.el ends here
