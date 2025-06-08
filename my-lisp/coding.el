;; Config for coding and data science
;; started on June 8th, 2025
;; Author T. Kasuga

;; coding.el --- Emacs programming environment configuration
;; Author: T. Kasuga

;;; Commentary:
;; Programming-related configuration including Python, R, and LSP.

;;; Code:

;; Flymake（構文チェック）
(use-package flymake
  :ensure t
  :hook ((python-mode . flymake-mode)
         (ess-r-mode   . flymake-mode)))  ;; R用

;; LSP（eglotを使用）
(use-package eglot
  :ensure t
  :commands eglot
  :hook ((python-mode . eglot-ensure)
         (ess-r-mode   . eglot-ensure)))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((python-mode) . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((ess-r-mode) . ("R" "--slave" "-e" "languageserver::run()"))))

;; Python設定
(use-package python-mode
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :hook (python-mode . eglot-ensure)
  :custom
  (python-indent-offset 4))

;; ESS（R言語）
(use-package ess
  :ensure t
  :mode ("\\.R\\'" . R-mode))

(provide 'coding)
;;; coding.el ends here


;; (use-package flymake
;;   :ensure t)  ;; Emacs組み込みだが Elpacaで最新版を明示的に使う

;; ;; Pythonモード
;; (use-package python-mode
;;   :ensure t
;;   :mode ("\\.py\\'" . python-mode)
;;   :hook (python-mode . eglot-ensure)) ;; ← 後述のLSPとの連携


;; (use-package eglot
;;   :ensure t
;;   :commands eglot
;;   :hook ((python-mode . eglot-ensure)
;;          (ess-r-mode   . eglot-ensure)))



;; (use-package ess
;;   :ensure t
;;   :mode ("\\.R\\'" . R-mode)
;;   :hook (ess-r-mode . eglot-ensure))

;; (setq eldoc-echo-area-use-multiline-p nil)  ;; 1行で表示したい場合


;; (provide 'coding)
