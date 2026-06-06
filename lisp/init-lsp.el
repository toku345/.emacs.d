;;; init-lsp.el --- LSP, flymake, tree-sitter -*- lexical-binding: t; -*-
;;; Commentary:
;;; LSP は組込の eglot（旧 lsp-mode + lsp-ui）。
;;; 文法チェックは eglot と統合される flymake（旧 flycheck）。
;;; 構文解析は tree-sitter（treesit-auto で文法を自動導入し *-ts-mode へ remap）。
;;; Code:

;;; --- eglot（組込 LSP クライアント） ---
(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :bind (:map eglot-mode-map
              ("C-c h" . eldoc-doc-buffer)   ; 旧 lsp-describe-thing-at-point
              ("C-c r" . eglot-rename)
              ("C-c a" . eglot-code-actions))
  :config
  (setq eglot-autoshutdown t          ; 最後のバッファを閉じたらサーバ停止
        eglot-events-buffer-size 0    ; イベントログを無効化して軽量化
        eglot-sync-connect 1)
  ;; Swift（旧 lsp-sourcekit）。パスがあれば登録。
  (let ((sourcekit "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))
    (when (file-exists-p sourcekit)
      (add-to-list 'eglot-server-programs
                   `((swift-mode swift-ts-mode) . (,sourcekit))))))

;;; --- flymake（組込の文法チェック） ---
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)))

;;; --- tree-sitter（組込 treesit + 文法自動導入） ---
(use-package treesit-auto
  :config
  ;; 文法が無ければ導入を確認し、対応モードを *-ts-mode へ自動 remap
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode 1))

;;; tree-sitter のフォントロックを最大レベルに
(setq treesit-font-lock-level 4)

(provide 'init-lsp)
;;; init-lsp.el ends here
