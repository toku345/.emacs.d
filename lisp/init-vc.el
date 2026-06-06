;;; init-vc.el --- version control -*- lexical-binding: t; -*-
;;; Commentary:
;;; Git 連携。magit と、git-gutter の後継である diff-hl。
;;; Code:

(use-package magit
  :bind (("C-x M-g" . magit-dispatch)   ; 旧 magit-dispatch-popup（廃止）を修正
         ("C-c m"   . magit-status)
         ("C-c b"   . magit-blame))
  :config
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-todos
  :after magit
  :config
  (magit-todos-mode 1))

;;; 差分のフリンジ表示（git-gutter の後継。magit と綺麗に連携する）
(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :init
  (global-diff-hl-mode 1)
  :config
  ;; 編集中もリアルタイムに差分反映
  (diff-hl-flydiff-mode 1)
  ;; TTY ではフリンジが無いのでマージンに表示
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1))
  ;; magit 操作の前後で差分表示を更新
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  ;; ハンク操作（C-x p/n は project.el・narrow と衝突するため C-c g 配下へ）
  :bind (("C-c g n" . diff-hl-next-hunk)
         ("C-c g p" . diff-hl-previous-hunk)
         ("C-c g r" . diff-hl-revert-hunk)
         ("C-c g s" . diff-hl-show-hunk)))

(provide 'init-vc)
;;; init-vc.el ends here
