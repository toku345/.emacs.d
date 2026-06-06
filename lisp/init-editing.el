;;; init-editing.el --- editing helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;; 編集支援（括弧編集・スニペット・マルチカーソル・undo・折り畳み）。
;;; Code:

;;; --- Lisp 編集の共通設定（各 Lisp 系モードで使い回す） ---
(use-package paredit
  :defer t
  :bind (:map paredit-mode-map
              ("C-h" . paredit-backward-delete))
  :config
  (defun my/conditionally-enable-paredit-mode ()
    "eval-expression のミニバッファでのみ paredit を有効化。"
    (when (eq this-command 'eval-expression)
      (paredit-mode 1)))
  (add-hook 'minibuffer-setup-hook #'my/conditionally-enable-paredit-mode))

(use-package rainbow-delimiters
  :defer t)

(use-package eldoc
  :ensure nil
  :diminish
  :config
  (setq eldoc-idle-delay 0.1
        eldoc-minor-mode-string ""))

(defun my/lisp-mode-defaults ()
  "Lisp 系モード共通の設定。"
  (paredit-mode 1)
  (rainbow-delimiters-mode 1)
  (eldoc-mode 1))

(defun my/lisp-mode-hook ()
  "Lisp 系モードのフック本体。"
  (my/lisp-mode-defaults))

(add-hook 'emacs-lisp-mode-hook #'my/lisp-mode-hook)

;;; --- スニペット ---
(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1)
  :bind (:map yas-minor-mode-map
              ;; TAB は corfu/インデントに譲り、展開と衝突させない
              ("<tab>" . nil)
              ("TAB" . nil)
              ("C-i" . nil)))

;;; 実際のスニペット集
(use-package yasnippet-snippets
  :after yasnippet)

;;; --- マルチカーソル ---
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)))

;;; --- undo ---
;;; undo-tree は廃し、組込 undo-redo + vundo（視覚的 undo ツリー）に移行
(use-package vundo
  :bind (("C-x u" . vundo)
         ("C-'"   . undo-redo)))

;;; --- 折り畳み ---
(use-package yafolding
  :bind (:map global-map
              ("M-RET" . yafolding-toggle-element)))

(provide 'init-editing)
;;; init-editing.el ends here
