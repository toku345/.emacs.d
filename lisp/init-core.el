;;; init-core.el --- basic editor defaults -*- lexical-binding: t; -*-
;;; Commentary:
;;; エディタの基本的な既定値・挙動。特定パッケージに依存しない設定群。
;;; Code:

;;; macOS でシェルの PATH を引き継ぐ
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

;;; y/n で答えられるようにする（Emacs 28+ の正攻法。defalias は使わない）
(setq use-short-answers t)

;;; 各種既定値（旧 init.el より移植）
(setq visible-bell t
      ring-bell-function 'ignore
      echo-keystrokes 0.1
      create-lockfiles nil
      truncate-partial-width-windows nil
      kill-whole-line t                 ; C-k で改行ごと削除
      completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      frame-title-format "%f"
      uniquify-buffer-name-style 'post-forward)

;;; インデント: タブではなくスペース、幅 2
(setq-default tab-width 2
              indent-tabs-mode nil)

;;; バッファ端の表示
(set-default 'indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'right)

;;; 選択範囲を入力で置換
(delete-selection-mode 1)

;;; 括弧の自動補完（グローバル）
(electric-pair-mode 1)

;;; 対応括弧の強調
(use-package paren
  :ensure nil
  :init
  (setq show-paren-style 'parenthesis)
  :config
  (show-paren-mode 1))

;;; CamelCase をサブワードとして扱う
(use-package subword
  :ensure nil
  :config
  (global-subword-mode 1))

;;; ファイル変更を自動で再読込
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

;;; #! で始まるファイルは保存時に +x を付与
(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

;;; GUI 起動時のみ emacsclient 用サーバを起動
(when (display-graphic-p)
  (require 'server)
  (unless (server-running-p)
    (server-start)))

;;; dired
(use-package dired
  :ensure nil
  :config
  (setq dired-dwim-target t            ; 2 画面 dired でコピー/移動先を賢く
        dired-recursive-copies 'always
        dired-isearch-filenames t
        dired-use-ls-dired t
        dired-listing-switches "-alh")
  ;; macOS の ls はオプション非対応なので coreutils の gls を使う
  (let ((gls "/usr/local/bin/gls"))
    (when (file-exists-p gls)
      (setq insert-directory-program gls))))

;;; バックアップ・オートセーブを ~/.emacs.d/backups/ に集約
(setq backup-inhibited nil
      delete-auto-save-files t
      auto-save-timeout 15
      auto-save-interval 60)
(let ((backup-dir (expand-file-name "backups/" user-emacs-directory)))
  (unless (file-directory-p backup-dir)
    (make-directory backup-dir t))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,backup-dir t))))

;;; 最近開いたファイル・ミニバッファ履歴
(use-package recentf
  :ensure nil
  :init
  (setq recentf-max-saved-items 300
        recentf-auto-cleanup 'never)
  :config
  (recentf-mode 1))

(use-package savehist
  :ensure nil
  :config
  (savehist-mode 1))

(use-package saveplace
  :ensure nil
  :config
  (save-place-mode 1))

;;; EditorConfig（プロジェクト間のスタイル統一）
(use-package editorconfig
  :config
  (editorconfig-mode 1))

(provide 'init-core)
;;; init-core.el ends here
