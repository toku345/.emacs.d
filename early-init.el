;;; early-init.el --- early initialization -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs 27+ で init.el より前に読み込まれる。フレーム生成前に行うべき
;;; 起動高速化・native-comp・UI 抑制をここで設定する。
;;; Code:

;;; 起動中は GC をほぼ無効化して高速化し、起動後に現実的な値へ戻す
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024) ; 64MB
                  gc-cons-percentage 0.1)))

;;; ファイルハンドラを起動中だけ退避（読み込み高速化）
(defvar my/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist my/file-name-handler-alist)))

;;; native-compilation（Emacs 28+）
(when (featurep 'native-compile)
  ;; 警告・エラーをミニバッファに出さずログに留める
  (setq native-comp-async-report-warnings-errors 'silent)
  ;; 遅延 JIT ネイティブコンパイルを有効化（29.1+ の名称）
  (when (boundp 'native-comp-jit-compilation)
    (setq native-comp-jit-compilation t))
  ;; eln キャッシュの配置
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (expand-file-name "var/eln-cache/" user-emacs-directory))))

;;; UI 要素はフレーム生成前に無効化してちらつきを防ぐ
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;; 起動画面・暗黙のリサイズを抑制
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      ;; package.el はこちらで初期化制御する（init-package.el で package-initialize）
      package-enable-at-startup t
      ;; default-frame の初期見た目（フレーム生成時のちらつき防止）
      default-frame-alist '((tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (vertical-scroll-bars . nil)))

;;; early-init では custom が早期に書き換えるのを避ける
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'early-init)
;;; early-init.el ends here
