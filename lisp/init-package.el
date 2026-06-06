;;; init-package.el --- package.el + use-package -*- lexical-binding: t; -*-
;;; Commentary:
;;; パッケージアーカイブと use-package の基本設定。
;;; use-package は Emacs 29+ に同梱されているため install 不要。
;;; Code:

(require 'package)

;;; アーカイブ: GNU ELPA / NonGNU ELPA / MELPA
;;; 多くのモダンパッケージ（vertico/corfu 等）は melpa・nongnu にある。
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))

;;; アーカイブ優先度（基本は melpa を優先。安定性が要るものは個別 :pin）
(setq package-archive-priorities
      '(("gnu"    . 10)
        ("nongnu" . 8)
        ("melpa"  . 6)))

(unless (bound-and-true-p package--initialized)
  (package-initialize))

;;; アーカイブ情報が無ければ取得（初回のみ）
(unless package-archive-contents
  (ignore-errors (package-refresh-contents)))

;;; use-package は同梱（古い Emacs 向けのフォールバックのみ残す）
(unless (require 'use-package nil t)
  (package-install 'use-package)
  (require 'use-package))

;;; use-package 既定動作
;;; - 旧設定の `use-package-always-pin "melpa-stable"` は撤廃。
;;;   stable 未提供パッケージが多いため melpa ローリングを既定にする。
(setq use-package-always-ensure t      ; :ensure を都度書かなくて済む
      use-package-expand-minimally t   ; 展開コードを簡潔に
      use-package-compute-statistics nil)

;;; 外部コマンド依存を宣言できるようにする（任意・存在すれば利用）
(use-package use-package-ensure-system-package
  :ensure t)

(provide 'init-package)
;;; init-package.el ends here
