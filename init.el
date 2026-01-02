;;; init.el --- Emacs configuration entry point -*- lexical-binding: t -*-

;; 1. カスタム設定ファイルを読み込む場所 (~/.emacs.d/lisp) を追加
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; 2. 各機能ファイルを順番に読み込む
;;    もし特定のエラーが出たら、その行の先頭に ; をつけてコメントアウトすれば無効化できます。

(require 'init-base)  ; 基礎設定 (Package, Font, Mac, etc)
(require 'init-ui)    ; UI・操作 (Vertico, Theme, Window)
(require 'init-org)   ; 執筆・思考 (Org, PDF, Denote)
(require 'init-dev)   ; 開発 (Magit, Eglot, Treesit)

;; 3. カスタムファイル (Emacsが自動生成する設定) の読み込み
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
