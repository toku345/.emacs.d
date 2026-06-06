;;; init-completion.el --- modern completion stack -*- lexical-binding: t; -*-
;;; Commentary:
;;; 補完スタックを現代化。
;;;   ミニバッファ補完: Vertico + Marginalia + Orderless + Consult + Embark
;;;   バッファ内補完:   Corfu + Cape
;;; 旧構成（Helm + company + swiper）からの置き換え。
;;; Code:

;;; --- ミニバッファ縦型補完 ---
(use-package vertico
  :init
  (vertico-mode 1)
  :config
  (setq vertico-cycle t
        vertico-count 15))

;;; 直前の補完セッションを再開（旧 ivy-resume 相当）
(use-package vertico-repeat
  :ensure nil
  :after vertico
  :bind ("C-c C-r" . vertico-repeat)
  :init
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

;;; 候補の補足情報（ファイル属性・docstring 等）
(use-package marginalia
  :init
  (marginalia-mode 1))

;;; あいまい・順不同マッチ
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;;; 横断的な検索・ナビゲーションコマンド群（Helm 各種の置き換え）
(use-package consult
  :bind (("M-y"     . consult-yank-pop)        ; 旧 helm-show-kill-ring
         ("C-x b"   . consult-buffer)          ; バッファ切替
         ("C-x C-b" . consult-buffer)          ; 旧 helm-for-files
         ("C-s"     . consult-line)            ; 旧 swiper
         ("M-i"     . consult-line)            ; 旧 helm-swoop
         ("M-I"     . consult-line-multi)      ; 旧 helm-multi-swoop-all
         ("C-x M-i" . consult-line-multi)
         ("C-c o"   . consult-line)            ; 旧 helm-occur
         ("M-g g"   . consult-goto-line)
         ("M-g i"   . consult-imenu)
         ("M-s r"   . consult-ripgrep)         ; プロジェクト全文検索
         ("M-s g"   . consult-grep))
  :init
  ;; xref のプレビュー等に consult を利用
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-narrow-key "<"))

;;; アクションメニュー（候補に対する操作）。consult との統合も。
(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings)))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;; --- バッファ内（補完点）補完 ---
(use-package corfu
  :init
  (global-corfu-mode 1)
  :config
  (setq corfu-cycle t
        corfu-auto t                 ; 自動ポップアップ
        corfu-auto-delay 0.2         ; 旧 company-idle-delay 相当
        corfu-auto-prefix 2          ; 旧 company-minimum-prefix-length 相当
        corfu-preselect 'prompt)
  ;; 補完点でのインデント/補完の切り替え
  (setq tab-always-indent 'complete)
  :bind (:map corfu-map
              ("C-n" . corfu-next)
              ("C-p" . corfu-previous)))

;;; 候補のドキュメントをポップアップ表示
(use-package corfu-popupinfo
  :ensure nil
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode))

;;; ターミナル（非 GUI）でも Corfu のポップアップを表示
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :after corfu
  :config
  (corfu-terminal-mode 1))

;;; 補完関数（completion-at-point-functions）の拡充
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(provide 'init-completion)
;;; init-completion.el ends here
