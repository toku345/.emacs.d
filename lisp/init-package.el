;;; init-package.el --- package.el + use-package -*- lexical-binding: t; -*-
;;; Commentary:
;;; Basic package archives and use-package setup.
;;; use-package is bundled with Emacs 29+, so it usually needs no install.
;;; Code:

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

(unless (bound-and-true-p package--initialized)
  (package-initialize))

;;; Fetch archive contents only when missing, usually on first startup.
(unless package-archive-contents
  (ignore-errors (package-refresh-contents)))

;;; use-package is bundled; keep only the fallback for older Emacs versions.
(unless (require 'use-package nil t)
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
