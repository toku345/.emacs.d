;;; init-lsp.el --- LSP, flymake, tree-sitter -*- lexical-binding: t; -*-
;;; Commentary:
;;; LSP uses built-in eglot, replacing lsp-mode and lsp-ui.
;;; Syntax checking uses flymake, integrated with eglot, replacing flycheck.
;;; Parsing uses tree-sitter with treesit-auto grammar installation and remaps.
;;; Code:

;;; --- eglot, the built-in LSP client ---
(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :bind (:map eglot-mode-map
              ("C-c h" . eldoc-doc-buffer)   ; Old lsp-describe-thing-at-point.
              ("C-c r" . eglot-rename)
              ("C-c a" . eglot-code-actions))
  :config
  (setq eglot-autoshutdown t          ; Stop server after the last buffer closes.
        eglot-events-buffer-size 0    ; Disable event logs to keep eglot light.
        eglot-sync-connect 1)
  ;; Ruby: support ruby-lsp while keeping Eglot's solargraph default as fallback.
  (add-to-list 'eglot-server-programs
               `((ruby-mode ruby-ts-mode)
                 . ,(eglot-alternatives
                     '(("ruby-lsp")
                       ("solargraph" "socket" "--port" :autoport)))))
  ;; Swift, replacing lsp-sourcekit. Register it when the binary exists.
  (let ((sourcekit "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))
    (when (file-exists-p sourcekit)
      (add-to-list 'eglot-server-programs
                   `((swift-mode swift-ts-mode) . (,sourcekit))))))

;;; --- flymake, built-in syntax checking ---
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)))

;;; --- tree-sitter, built-in treesit plus automatic grammar installation ---
(use-package treesit-auto
  :config
  ;; Prompt for missing grammars and remap supported modes to *-ts-mode.
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode 1)
  (treesit-auto-add-to-auto-mode-alist
   '(dockerfile go json rust typescript tsx yaml)))

;;; Use the maximum tree-sitter font-lock level.
(setq treesit-font-lock-level 4)

(provide 'init-lsp)
;;; init-lsp.el ends here
