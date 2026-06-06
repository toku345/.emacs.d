;;; init.el --- main init: load modular config -*- lexical-binding: t; -*-
;;; Commentary:
;;; The configuration is split into modules. This file only sets load-path
;;; and requires the init-*.el modules under lisp/.
;;; Code:

;;; Add lisp/ to load-path.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;; Set character encoding before loading modules.
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;;; --- Module loading ---
;;; Require modules in dependency order. init-package prepares use-package.
(require 'init-package)      ; package.el archives and use-package setup
(require 'init-core)         ; core defaults, auto-revert, and dired
(require 'init-keys)         ; global key bindings
(require 'init-completion)   ; Vertico/Consult/Corfu completion stack
(require 'init-ui)           ; theme, modeline, and appearance
(require 'init-editing)      ; paredit, multiple-cursors, and yasnippet
(require 'init-vc)           ; magit and diff-hl
(require 'init-project)      ; project.el and search
(require 'init-lsp)          ; eglot, flymake, and tree-sitter
(require 'init-langs)        ; language modes
(require 'init-org)          ; org and easy-hugo
(require 'init-misc)         ; miscellaneous utilities

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
