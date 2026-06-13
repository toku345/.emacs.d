;;; early-init.el --- early initialization -*- lexical-binding: t; -*-
;;; Commentary:
;;; Loaded before init.el on Emacs 27+.  Keep startup optimizations,
;;; native compilation settings, and pre-frame UI suppression here.
;;; Code:

;;; Disable GC during startup, then restore practical values afterwards.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024) ; 64MB
                  gc-cons-percentage 0.1)))

;;; Temporarily disable file name handlers during startup for faster loading.
(defvar my/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist my/file-name-handler-alist)))

;;; native-compilation (Emacs 28+)
(when (featurep 'native-compile)
  ;; Keep native-comp warnings in logs instead of showing them in the minibuffer.
  (setq native-comp-async-report-warnings-errors 'silent)
  ;; Enable deferred JIT native compilation where available.
  (when (boundp 'native-comp-jit-compilation)
    (setq native-comp-jit-compilation t))
  ;; Place the eln cache under user-emacs-directory.
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (expand-file-name "var/eln-cache/" user-emacs-directory))))

;;; Disable UI elements before frame creation to avoid flicker.
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;; Suppress the startup screen and implicit frame resizing.
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      ;; Keep package activation before init.el; init-package.el sets archives.
      package-enable-at-startup t
      ;; Activate packages from one combined autoloads file instead of loading
      ;; ~100 per-package autoload files. package.el refreshes the file on
      ;; install/delete; run make package-quickstart after manual changes.
      package-quickstart t
      ;; Initial default-frame appearance, set early to avoid frame flicker.
      default-frame-alist '((tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (vertical-scroll-bars . nil)))

;;; Prevent Custom from writing too early during early-init.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'early-init)
;;; early-init.el ends here
