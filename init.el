;;; init.el --- main init: load modular config -*- lexical-binding: t; -*-
;;; Commentary:
;;; The configuration is split into modules.  This file only sets load-path
;;; and requires the init-*.el modules under lisp/.
;;; Code:

;;; Add lisp/ to load-path.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;; Set character encoding before loading modules.
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;;; --- Module loading ---
;;; Require modules in dependency order. init-package prepares use-package.
;;; Interactively a broken module logs a warning and the rest still loads,
;;; keeping the session repairable; in batch (make smoke) the error is
;;; re-signaled so the verification gate stays fail-fast.
(dolist (module '(init-package      ; package.el archives and use-package setup
                  init-core         ; core defaults, auto-revert, and dired
                  init-keys         ; global key bindings
                  init-completion   ; Vertico/Consult/Corfu completion stack
                  init-ui           ; theme, modeline, and appearance
                  init-editing      ; paredit, multiple-cursors, and yasnippet
                  init-vc           ; magit and diff-hl
                  init-project      ; project.el and search
                  init-lsp          ; eglot, flymake, and tree-sitter
                  init-langs        ; language modes
                  init-org          ; org and easy-hugo
                  init-misc))       ; miscellaneous utilities
  (condition-case err
      (require module)
    (error
     (if noninteractive
         (signal (car err) (cdr err))
       (display-warning 'init
                        (format "Failed to load %s: %s"
                                module (error-message-string err))
                        :error)))))

;;; Keep custom-set-* in custom.el, which is not version controlled.
(unless custom-file
  (setq custom-file (locate-user-emacs-file "custom.el")))
(load custom-file 'noerror)

;;; Report startup time.
(add-hook 'after-init-hook
          (lambda ()
            (message "init time: %.3f sec"
                     (float-time (time-subtract after-init-time before-init-time)))))

(provide 'init)
;;; init.el ends here
