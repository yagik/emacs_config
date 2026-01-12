;;; init-dev.el --- Programming & Development -*- lexical-binding: t -*-

;; 1. Git連携 (Magit)
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))


;; 2. 【自家製】VBA/Basic用カラー設定 (パッケージ不要)
;; 外部サーバーからダウンロードせず、Emacsの標準機能だけで色を付けます。
;; これにより「Package unavailable」エラーを根本的に回避します。


;; 3. 洗練された補完 UI (Corfu)
;; 入力中に邪魔にならない程度に候補を表示してくれる、モダンな補完ツールです
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)                 ; 自動で候補を出す
  (corfu-auto-delay 0.1)         ; 0.1秒待って表示
  (corfu-auto-prefix 2)          ; 2文字打ったら表示
  (corfu-quit-at-boundary nil)   ; スペースを打っても消えない
  :init
  (global-corfu-mode))

;; 4. 構文チェックの標準化 (Flymake)
;; Emacs標準の機能を使って、コードの書き間違いを警告します
(add-hook 'prog-mode-hook 'flymake-mode)

;; 5. Tree-sitter & Eglot (以前の設定を維持)
(use-package treesit
  :ensure nil
  :if (boundp 'treesit-font-lock-level)
  :config
  (setq treesit-font-lock-level 4))

(use-package eglot
  :ensure nil
  :hook
  ((python-ts-mode . eglot-ensure)
   (js-ts-mode . eglot-ensure))
  :config
  (keymap-set eglot-mode-map "s-." 'eglot-code-actions))

;; 6. 変数のハイライト
(use-package symbol-overlay
  :ensure t
  :bind ("M-i" . symbol-overlay-put))

(provide 'init-dev)
;;; init-dev.el ends here
