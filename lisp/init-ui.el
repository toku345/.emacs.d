;;; init-ui.el --- appearance & modeline -*- lexical-binding: t; -*-
;;; Commentary:
;;; 見た目（テーマ・モードライン・空白表示・行ハイライト等）。
;;; Code:

;;; テーマ（zenburn を踏襲。組込の modus-themes に乗り換える場合は
;;; (load-theme 'modus-vivendi t) で置き換え可能）
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t)
  ;; 旧設定の色味を踏襲
  (with-eval-after-load 'hl-line
    (set-face-background 'hl-line "#525252"))
  (set-face-background 'region "dark green"))

;;; モードライン
(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t
        sml/theme 'respectful
        sml/shorten-directory -1)
  :config
  (sml/setup)
  (column-number-mode 1)
  (line-number-mode 1))

(display-time-mode 1)

;;; マイナーモード表示を簡潔に
(use-package diminish)

;;; 空白の可視化（旧設定の表示マッピングを踏襲）
(use-package whitespace
  :ensure nil
  :diminish global-whitespace-mode
  :config
  (setq whitespace-style '(face trailing tabs spaces empty space-mark tab-mark)
        whitespace-display-mappings
        '((space-mark ?　 [?□])
          (tab-mark ?\t [?» ?\t] [?\\ ?\t]))
        whitespace-space-regexp "\\(\x3000+\\|^ +\\| +$\\)")
  (global-whitespace-mode 1))

;;; 保存時に行末の余分な空白を除去
(add-hook 'before-save-hook #'whitespace-cleanup)

;;; 現在行のハイライト（モダンに global-hl-line-mode へ簡素化）
(use-package hl-line
  :ensure nil
  :config
  (global-hl-line-mode 1))

;;; シンボルのハイライト/ナビゲーション（highlight-symbol の後継）
(use-package symbol-overlay
  :diminish
  :hook (prog-mode . symbol-overlay-mode)
  :bind (("M-s h" . symbol-overlay-put)
         ("M-s n" . symbol-overlay-jump-next)
         ("M-s p" . symbol-overlay-jump-prev)))

(provide 'init-ui)
;;; init-ui.el ends here
