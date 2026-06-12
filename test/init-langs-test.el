;;; init-langs-test.el --- tests for init-langs -*- lexical-binding: t; -*-
;;; Commentary:
;;; ERT tests for language-mode dispatch.  Run after loading the full
;;; configuration so package activation is in effect:
;;;   emacs -Q --batch -l early-init.el -l init.el \
;;;     -l test/init-langs-test.el -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)

(defvar geiser-active-implementations)

(ert-deftest init-langs-test-scss-mode-dispatch ()
  "A .scss file dispatches to the working built-in `scss-mode'.
Guards against a stale MELPA `scss-mode' package shadowing the
built-in one: its autoloads win over css-mode.el and its body
fails to load on Emacs 30, leaving the buffer in `fundamental-mode'."
  (with-temp-buffer
    (setq buffer-file-name "init-langs-test.scss")
    (set-auto-mode)
    (should (eq major-mode 'scss-mode))))

(ert-deftest init-langs-test-scheme-mode-uses-racket-geiser ()
  "Entering `scheme-mode' uses Racket without prompting for an implementation."
  (with-temp-buffer
    (scheme-mode)
    (should (eq major-mode 'scheme-mode))
    (should (equal geiser-active-implementations '(racket)))))

(provide 'init-langs-test)
;;; init-langs-test.el ends here
