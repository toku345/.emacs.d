;;; init-package.el --- package.el + use-package -*- lexical-binding: t; -*-
;;; Commentary:
;;; Basic package archives and use-package setup.
;;; use-package is bundled with Emacs 29+, so it usually needs no install.
;;; Code:

(require 'package)

;;; Fail hard on TLS certificate problems instead of falling back to the
;;; interactive NSM prompt, which batch runs cannot answer. GNU/NonGNU
;;; archives are additionally signature-checked (package-check-signature
;;; defaults to allow-unsigned); MELPA ships unsigned, so certificate
;;; verification is its only integrity guarantee. This is global for every
;;; TLS connection Emacs makes, which is the intent.
(setq gnutls-verify-error t)

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

;;; Skip initialization when startup.el already activated packages, fully
;;; (package--initialized) or via the quickstart file (package--activated);
;;; re-running package-initialize would repeat the whole activation.
(unless (or (bound-and-true-p package--initialized)
            (bound-and-true-p package--activated))
  (package-initialize))

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
