;;; init-project.el --- project & search -*- lexical-binding: t; -*-
;;; Commentary:
;;; プロジェクト管理は組込の project.el（旧 projectile）。検索系もここに集約。
;;; Code:

;;; 組込 project.el。旧 projectile のキー（C-c p / s-p）も使えるようにする。
(use-package project
  :ensure nil
  :bind-keymap (("C-c p" . project-prefix-map)
                ("s-p"   . project-prefix-map)))

;;; grep バッファを直接編集して反映（consult/embark の export と相性が良い）
(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

;;; 定義ジャンプのフォールバック（xref バックエンドとして登録）
(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :config
  (setq dumb-jump-prefer-searcher 'rg))

(provide 'init-project)
;;; init-project.el ends here
