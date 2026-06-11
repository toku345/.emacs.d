;;; init-completion.el --- modern completion stack -*- lexical-binding: t; -*-
;;; Commentary:
;;; Modern completion stack.
;;;   Minibuffer completion: Vertico + Marginalia + Orderless + Consult + Embark
;;;   In-buffer completion:  Corfu + Cape
;;; Replacement for the old Helm + company + swiper setup.
;;; Code:

;;; --- Vertical minibuffer completion ---
(use-package vertico
  :init
  (vertico-mode 1)
  :config
  (setq vertico-cycle t
        vertico-count 15))

;;; Resume the previous completion session, replacing ivy-resume.
(use-package vertico-repeat
  :ensure nil
  :after vertico
  :bind ("C-c C-r" . vertico-repeat)
  :init
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

;;; Candidate annotations such as file attributes and docstrings.
(use-package marginalia
  :init
  (marginalia-mode 1))

;;; Flexible and orderless matching.
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;;; Cross-cutting search and navigation commands, replacing Helm commands.
(use-package consult
  :bind (("M-y"     . consult-yank-pop)        ; Old helm-show-kill-ring.
         ("C-x b"   . consult-buffer)          ; Buffer switching.
         ("C-x C-b" . consult-buffer)          ; Old helm-for-files.
         ("C-s"     . consult-line)            ; Old swiper.
         ("M-i"     . consult-line)            ; Old helm-swoop.
         ("M-I"     . consult-line-multi)      ; Old helm-multi-swoop-all.
         ("C-x M-i" . consult-line-multi)
         ("C-c o"   . consult-line)            ; Old helm-occur.
         ("M-g g"   . consult-goto-line)
         ("M-g i"   . consult-imenu)
         ("M-s r"   . consult-ripgrep)         ; Project-wide full-text search.
         ("M-s g"   . consult-grep))
  :init
  ;; Use consult for xref previews.
  (setopt xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-narrow-key "<"))

;;; Action menu for candidates, with consult integration.
(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;; --- In-buffer completion at point ---
(use-package corfu
  :init
  (global-corfu-mode 1)
  :custom
  (corfu-cycle t)
  (corfu-auto t)                 ; Show popup automatically.
  (corfu-auto-delay 0.2)         ; Old company-idle-delay equivalent.
  (corfu-auto-prefix 2)          ; Old company-minimum-prefix-length equivalent.
  (corfu-preselect 'prompt)
  :config
  ;; Switch between indentation and completion at point.
  (setq tab-always-indent 'complete)
  :bind (:map corfu-map
              ("C-n" . corfu-next)
              ("C-p" . corfu-previous)))

;;; Show candidate documentation in a popup.
(use-package corfu-popupinfo
  :ensure nil
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode))

;;; Show Corfu popups in terminal sessions.
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :after corfu
  :config
  (corfu-terminal-mode 1))

;;; Add extra completion-at-point functions.
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(provide 'init-completion)
;;; init-completion.el ends here
