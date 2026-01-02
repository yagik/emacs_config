;;; init-org.el --- Org, PDF, Writing tools -*- lexical-binding: t -*-

(global-set-key (kbd "C-c l") 'org-store-link)

;; 日本語検索 (Migemo)
(use-package migemo
  :if (executable-find "cmigemo")
  :config
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/opt/homebrew/share/migemo/utf-8/migemo-dict")
  (migemo-init))

;; Org-mode 外見設定関数
(defun my/org-mode-visual-settings ()
  "Org-modeの本文をヒラギノに、表などを等幅にする"
  (face-remap-add-relative 'default :family "Hiragino Kaku Gothic Pro" :height 180)
  (set-face-attribute 'org-table nil :family "PlemolJP" :height 160)
  (set-face-attribute 'org-block nil :family "PlemolJP" :height 160)
  (set-face-attribute 'org-code nil :family "PlemolJP" :height 160)
  (set-face-attribute 'org-verbatim nil :family "PlemolJP" :height 160)
  (setq-local line-spacing 0.2))

;; Org-mode 本体
(use-package org
  :hook (org-mode . my/org-mode-visual-settings)
  :custom
  (org-startup-indented t)
  (org-hide-emphasis-markers t))

(use-package olivetti
  :hook (org-mode . olivetti-mode)
  :custom (olivetti-body-width 90))

;; PDF Tools 設定
(use-package pdf-tools
  :ensure t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-loader-install)
  (with-eval-after-load 'pdf-view
    (define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
    (define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)
    (define-key pdf-view-mode-map (kbd "l") 'image-forward-hscroll)
    (define-key pdf-view-mode-map (kbd "h") 'image-backward-hscroll)
    (define-key pdf-view-mode-map (kbd "o") 'pdf-occur))
  (setq pdf-view-use-scaling t)
  (setq-default pdf-view-display-size 'fit-width)
  (setq pdf-view-continuous t)
  (add-hook 'pdf-view-mode-hook (lambda () (line-number-mode -1))))


;; 1. Org-mode側のルールを矯正する
;;    デフォルトだと「ファイルリンクは別窓で開く」設定になっているため、
;;    これを「find-file (現在の窓で開く)」に強制変更します。
(setq org-link-frame-setup '((file . find-file)))

;; 2. Emacs全体のルールを強制適用するスイッチ
(setq switch-to-buffer-obey-display-actions t)

;; 3. ファイルの種類ごとの表示ルール (display-buffer-alist)
(setq display-buffer-alist
      '(
        ;; 【ルールA】 PDFファイル (.pdf)
        ;; → 右側のサイドウィンドウに隔離して表示
        ("\\.pdf\\'"
         (display-buffer-in-side-window)
         (side . right)
         (slot . 0)
         (window-width . 0.5)
         (preserve-size . (t . t)))

        ;; 【ルールB】 Orgファイル (.org)
        ;; → Denoteなども含め、絶対に「今のウィンドウ」で開く
        ("\\.org$"
         (display-buffer-same-window)
         (inhibit-same-window . nil)) ; 「同じ窓禁止」フラグを明示的にOFFにする
       ))


(use-package org-pdftools
  :ensure t
  :hook (org-mode . org-pdftools-setup-link)
  :config (org-pdftools-setup-link))

;; Org周辺ツール
(use-package org-download
  :after org
  :config (org-download-enable))

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :custom (org-modern-table nil))

(use-package denote
  :hook (dired-mode . denote-dired-mode)
  :custom
  (denote-directory "~/Documents/denote")
  (denote-file-type 'org)
  :config (denote-rename-buffer-mode 1)
  :bind (("C-c n n" . denote)
         ("C-c n f" . denote-open-or-create)))

(provide 'init-org)
;;; init-org.el ends here
