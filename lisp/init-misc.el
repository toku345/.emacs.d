;;; init-misc.el --- misc utilities -*- lexical-binding: t; -*-
;;; Commentary:
;;; その他のユーティリティ。特定言語に紐づかない便利パッケージ群。
;;; Code:

;;; シンボルアウトライン
(use-package imenu-list
  :bind ("C-o" . imenu-list-smart-toggle))

;;; 一時ファイルを素早く作る
(use-package open-junk-file
  :bind ("C-x j" . open-junk-file)
  :custom
  (open-junk-file-format "~/works/junk/%Y/%m/%d-%H_%M_%S."))

;;; dired からのクイックプレビュー
(use-package quick-preview
  :bind (("C-c q" . quick-preview-at-point)
         :map dired-mode-map
         ("Q" . quick-preview-at-point)))

;;; ブラウザのテキストエリアを Emacs で編集
(use-package edit-server
  :commands edit-server-start
  :init
  (when (display-graphic-p)
    (add-hook 'emacs-startup-hook #'edit-server-start))
  :config
  (setq edit-server-new-frame nil))

;;; プレゼンテーション（文字を大きく表示）
(use-package presentation
  :commands presentation-mode)

;;; MediaWiki 編集
(use-package mediawiki
  :commands mediawiki-open)

;;; esa.io 連携
(use-package esa
  :commands (esa esa-list)
  :config
  (setq esa-token (getenv "ESA_ACCESS_TOKEN")
        esa-team-name (getenv "ESA_TEAM_NAME")))

(provide 'init-misc)
;;; init-misc.el ends here
