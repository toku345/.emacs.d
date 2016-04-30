;; -*- mode: Emacs-Lisp ; Coding: utf-8 -*-
(set-language-environment "UTF-8")

;; 入力されるキーシーケンスを置き換える
;; ?\C-?はDELのキーシーケンス
(keyboard-translate ?\C-h ?\C-?)
