;;; init-dev.el --- Programming & Development -*- lexical-binding: t -*-

;; Git連携 (Magit)
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; Tree-sitter (構文解析)
(use-package treesit
  :ensure nil
  :if (boundp 'treesit-font-lock-level)
  :config
  (setq treesit-font-lock-level 4))

;; Eglot (LSPクライアント)
(use-package eglot
  :ensure nil
  :hook
  ((python-ts-mode . eglot-ensure)
   (js-ts-mode . eglot-ensure))
  :config
  (keymap-set eglot-mode-map "s-." 'eglot-code-actions))

;; Symbol Overlay (変数ハイライト)
(use-package symbol-overlay
  :ensure t
  :bind ("M-i" . symbol-overlay-put))

(provide 'init-dev)
;;; init-dev.el ends here
