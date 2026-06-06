;;; init.el --- main init: load modular config -*- lexical-binding: t; -*-
;;; Commentary:
;;; 設定はモジュール分割し、本ファイルは load-path 設定と各モジュールの
;;; 読み込みのみを行う。各モジュールは lisp/ 配下の init-*.el。
;;; Code:

;;; lisp/ を load-path に追加
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;; 文字コード（最優先で確定させておく）
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;;; --- モジュール読み込み ---
;;; 依存順に require する。init-package が use-package を整える土台。
(require 'init-package)      ; package.el アーカイブ + use-package 設定
(require 'init-core)         ; 基本設定（既定値・auto-revert 等）
(require 'init-keys)         ; グローバルキーバインド
(require 'init-completion)   ; Vertico/Consult/Corfu 系 補完スタック
(require 'init-ui)           ; テーマ・modeline・見た目
(require 'init-editing)      ; 編集支援（括弧・マルチカーソル・yasnippet 等）
(require 'init-vc)           ; magit・diff-hl
(require 'init-project)      ; project.el・dired・検索
(require 'init-lsp)          ; eglot・flymake・tree-sitter
(require 'init-langs)        ; 各言語モード
(require 'init-org)          ; org・easy-hugo
(require 'init-misc)         ; その他ユーティリティ

;;; custom-set-* は custom.el に退避（バージョン管理外）
(load custom-file 'noerror)

;;; 起動時間の表示
(add-hook 'after-init-hook
          (lambda ()
            (message "init time: %.3f sec"
                     (float-time (time-subtract after-init-time before-init-time)))))

(provide 'init)
;;; init.el ends here
