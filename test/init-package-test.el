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
(declare-function my/package-quickstart-refresh-with-activation-check "init-package")

(defvar init-package-test-quickstart-loaded nil)

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

(ert-deftest init-package-test-quickstart-refresh-advice-installed ()
  "Automatic quickstart refresh uses the activation failure check."
  (should
   (advice-member-p #'my/package-quickstart-refresh-with-activation-check
                    'package-quickstart-refresh)))

(ert-deftest init-package-test-quickstart-refresh-fails-on-activation-error ()
  "Checked quickstart refresh fails when refresh hides activation errors."
  (let ((package-quickstart-called nil)
        (package-quickstart-file
         (make-temp-file "package-quickstart-failure" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file package-quickstart-file
            (insert "broken quickstart"))
          (with-temp-file (concat package-quickstart-file "c")
            (insert "broken compiled quickstart"))
          (cl-letf (((symbol-function 'package-activate)
                     (lambda (&rest _args)
                       (error "Simulated activation failure"))))
            (should-error
             (my/package-quickstart-refresh-with-activation-check
              (lambda ()
                (setq package-quickstart-called t)
                (condition-case err
                    (package-activate 'broken-package)
                  (error
                   (message "%s" (error-message-string err))))))))
          (should package-quickstart-called)
          (should-not (file-exists-p package-quickstart-file))
          (should-not (file-exists-p (concat package-quickstart-file "c"))))
      (dolist (file (list package-quickstart-file
                          (concat package-quickstart-file "c")))
        (when (file-exists-p file)
          (delete-file file))))))

(ert-deftest init-package-test-quickstart-refresh-succeeds-without-activation-error ()
  "Checked quickstart refresh keeps a loadable file on success."
  (let ((activation-called nil)
        (package-quickstart-file
         (make-temp-file "package-quickstart-success" nil ".el")))
    (unwind-protect
        (progn
          (delete-file package-quickstart-file)
          (cl-letf (((symbol-function 'package-activate)
                     (lambda (&rest _args)
                       (setq activation-called t))))
            (my/package-quickstart-refresh-with-activation-check
             (lambda ()
               (package-activate 'working-package)
               (with-temp-file package-quickstart-file
                 (insert "(setq init-package-test-quickstart-loaded t)\n")))))
          (should activation-called)
          (should (file-exists-p package-quickstart-file))
          (let ((init-package-test-quickstart-loaded nil))
            (load package-quickstart-file nil t)
            (should init-package-test-quickstart-loaded)))
      (dolist (file (list package-quickstart-file
                          (concat package-quickstart-file "c")))
        (when (file-exists-p file)
          (delete-file file))))))

(provide 'init-package-test)
;;; init-package-test.el ends here
