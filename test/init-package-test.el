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

(declare-function my/package-quickstart-batch-check "package-quickstart-batch")
(declare-function my/package-quickstart-batch-check-configured-file
                  "package-quickstart-batch")
(declare-function my/package-quickstart-batch-readable-file
                  "package-quickstart-batch")
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

(ert-deftest init-package-test-quickstart-check-requires-refresh-output ()
  "The batch check requires refresh to create the temporary quickstart file."
  (let ((refresh-called nil))
    (cl-letf (((symbol-function 'my/package-quickstart-batch-check-configured-file)
               #'ignore)
              ((symbol-function 'my/package-quickstart-batch-refresh)
               (lambda ()
                 (setq refresh-called t)
                 (should-not (file-exists-p package-quickstart-file))
                 (with-temp-file package-quickstart-file
                   (insert "(setq init-package-test-quickstart-loaded t)\n")))))
      (let ((init-package-test-quickstart-loaded nil))
        (my/package-quickstart-batch-check)
        (should refresh-called)
        (should init-package-test-quickstart-loaded)))))

(ert-deftest init-package-test-quickstart-check-loads-configured-file ()
  "The batch check verifies the configured quickstart file when present."
  (let ((package-quickstart-file
         (make-temp-file "package-quickstart-configured" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file package-quickstart-file
            (insert "(setq init-package-test-quickstart-loaded t)\n"))
          (let ((init-package-test-quickstart-loaded nil))
            (my/package-quickstart-batch-check-configured-file)
            (should init-package-test-quickstart-loaded)))
      (dolist (file (list package-quickstart-file
                          (concat package-quickstart-file "c")))
        (when (file-exists-p file)
          (delete-file file))))))

(ert-deftest init-package-test-quickstart-check-prefers-compiled-file ()
  "The batch check matches startup by preferring compiled quickstart files."
  (let ((package-quickstart-file
         (make-temp-file "package-quickstart-compiled" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file package-quickstart-file
            (insert ";;; package-quickstart-test.el -*- lexical-binding: t; -*-\n"
                    "(setq init-package-test-quickstart-loaded 'compiled)\n"))
          (byte-compile-file package-quickstart-file)
          (with-temp-file package-quickstart-file
            (insert "(setq init-package-test-quickstart-loaded 'source)\n"))
          (set-file-times package-quickstart-file 0)
          (should (string= (my/package-quickstart-batch-readable-file)
                           (concat package-quickstart-file "c")))
          (let ((init-package-test-quickstart-loaded nil))
            (my/package-quickstart-batch-check-configured-file)
            (should (eq init-package-test-quickstart-loaded 'compiled))))
      (dolist (file (list package-quickstart-file
                          (concat package-quickstart-file "c")))
        (when (file-exists-p file)
          (delete-file file))))))

(ert-deftest init-package-test-quickstart-check-fails-on-broken-configured-file ()
  "The batch check fails instead of ignoring a broken configured quickstart file."
  (let ((package-quickstart-file
         (make-temp-file "package-quickstart-broken" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file package-quickstart-file
            (insert "(invalid-read-syntax"))
          (should-error
           (my/package-quickstart-batch-check-configured-file)))
      (dolist (file (list package-quickstart-file
                          (concat package-quickstart-file "c")))
        (when (file-exists-p file)
          (delete-file file))))))

(ert-deftest init-package-test-quickstart-check-fails-on-broken-compiled-file ()
  "The batch check fails on a broken compiled quickstart file."
  (let ((package-quickstart-file
         (make-temp-file "package-quickstart-broken-compiled" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file package-quickstart-file
            (insert "(setq init-package-test-quickstart-loaded 'source)\n"))
          (with-temp-file (concat package-quickstart-file "c")
            (insert "(invalid-read-syntax"))
          (should (string= (my/package-quickstart-batch-readable-file)
                           (concat package-quickstart-file "c")))
          (let ((init-package-test-quickstart-loaded nil))
            (should-error
             (my/package-quickstart-batch-check-configured-file))
            (should-not init-package-test-quickstart-loaded)))
      (dolist (file (list package-quickstart-file
                          (concat package-quickstart-file "c")))
        (when (file-exists-p file)
          (delete-file file))))))

(provide 'init-package-test)
;;; init-package-test.el ends here
