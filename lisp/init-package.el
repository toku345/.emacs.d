;;; init-package.el --- package.el + use-package -*- lexical-binding: t; -*-
;;; Commentary:
;;; Basic package archives and use-package setup.
;;; use-package is bundled with Emacs 29+, so it usually needs no install.
;;; Code:

(require 'cl-lib)
(require 'package)

;;; Archives: GNU ELPA / NonGNU ELPA / MELPA.
;;; Many modern packages such as Vertico and Corfu live in MELPA or NonGNU.
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))

;;; Archive priorities. Pin individual packages when stability matters.
(setq package-archive-priorities
      '(("gnu"    . 10)
        ("nongnu" . 8)
        ("melpa"  . 6)))

;;; Initialize package metadata without repeating activation.
(defun my/package-initialize ()
  "Initialize package metadata, preserving startup package activation."
  (cond
   ((bound-and-true-p package--initialized)
    nil)
   ((bound-and-true-p package--activated)
    ;; Quickstart runs package activation before init.el, but does not populate
    ;; descriptor metadata such as `package-alist'.
    (package-initialize 'no-activate))
   (t
    (package-initialize))))

(my/package-initialize)

;;; Fail closed if quickstart refresh hides package activation errors.
(defun my/package-quickstart-delete-cache ()
  "Delete generated package quickstart files."
  (dolist (file (list package-quickstart-file
                      (concat package-quickstart-file "c")))
    (when (file-exists-p file)
      (delete-file file))))

(defun my/package-quickstart-refresh-with-activation-check (refresh-function
                                                            &rest args)
  "Call REFRESH-FUNCTION with ARGS, failing on hidden activation errors."
  (let ((activation-failures nil)
        (original-package-activate (symbol-function 'package-activate))
        result)
    (condition-case err
        (cl-letf (((symbol-function 'package-activate)
                   (lambda (package &optional force)
                     (condition-case activate-err
                         (funcall original-package-activate package force)
                       (error
                        (push (format "%s: %s"
                                      package
                                      (error-message-string activate-err))
                              activation-failures)
                        (signal (car activate-err) (cdr activate-err)))))))
          (setq result (apply refresh-function args)))
      (error
       (when activation-failures
         (my/package-quickstart-delete-cache))
       (signal (car err) (cdr err))))
    (if activation-failures
        (progn
          (my/package-quickstart-delete-cache)
          (error "Package quickstart activation failures: %s"
                 (mapconcat #'identity (nreverse activation-failures) "; ")))
      result)))

(advice-add 'package-quickstart-refresh
            :around #'my/package-quickstart-refresh-with-activation-check)

;;; use-package is bundled; keep only the fallback for older Emacs versions.
(unless (require 'use-package nil t)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'use-package)
  (require 'use-package))

;;; use-package defaults.
;;; - The old `use-package-always-pin "melpa-stable"` default is removed.
;;;   Many packages are not available on stable, so use rolling MELPA by default.
(setq use-package-always-ensure t      ; Avoid repeating :ensure everywhere.
      use-package-expand-minimally t   ; Keep generated expansion concise.
      use-package-compute-statistics nil)

(provide 'init-package)
;;; init-package.el ends here
