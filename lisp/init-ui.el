;;; init-ui.el --- UI & UX settings -*- lexical-binding: t -*-

;; テーマ設定
(use-package catppuccin-theme
  :ensure t
  :config
  (setq catppuccin-flavor 'mocha)
  (setq catppuccin-accent 'blue)
  (load-theme 'catppuccin t))

;; Org Headline colors (Spacemacs Dark-ish)
(defun my/org-headline-colors-spacemacs-dark ()
  "Set Org headline colors only (Spacemacs Dark-ish)."
  (interactive)
  ;; 文字色だけ変える（:weight や :height は触らない）
  (set-face-attribute 'org-level-1 nil :foreground "#4f97d7")
  (set-face-attribute 'org-level-2 nil :foreground "#2d9574")
  (set-face-attribute 'org-level-3 nil :foreground "#67b11d")
  (set-face-attribute 'org-level-4 nil :foreground "#b1951d")
  (set-face-attribute 'org-level-5 nil :foreground "#a31db1")
  (set-face-attribute 'org-level-6 nil :foreground "#bc6ec5")
  (set-face-attribute 'org-level-7 nil :foreground "#7590db")
  (set-face-attribute 'org-level-8 nil :foreground "#4f97d7"))

;; 起動時に適用（Orgがロードされた後）
(with-eval-after-load 'org
  (my/org-headline-colors-spacemacs-dark))

;; テーマを切り替えた後にも再適用（重要）
(add-hook 'after-load-theme-hook #'my/org-headline-colors-spacemacs-dark)


(use-package savehist
  :init
  (savehist-mode 1)
  (setq history-length 100))


;; モダンUI & 補完 (Vertico, Consult)
(use-package vertico :init (vertico-mode 1))
(use-package marginalia :init (marginalia-mode 1))
(use-package orderless :custom (completion-styles '(orderless basic)))

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("s-r"   . consult-recent-file)
         ("C-s"   . consult-line)
         ("s-f"   . consult-line))
  :config (setq consult-line-numbers-widen t))

;; ダッシュボード & タブ
(use-package tab-bar
  :ensure nil
  :init (tab-bar-mode 1)
  :custom (tab-bar-show t)
  (tab-bar-new-tab-choice "*dashboard*"))

(use-package dashboard
  :ensure t
  :init
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  :custom
  (dashboard-startup-banner (expand-file-name "img/Nyan_dashboard.png" user-emacs-directory))
  (dashboard-banner-logo-title "Wherever you go, there you are.")
  (dashboard-items '((recents . 10) (projects . 5) (bookmarks . 5)))
  (dashboard-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  :config (dashboard-setup-startup-hook))

;; 編集効率化とace-window
(use-package which-key :init (which-key-mode 1))

(use-package undo-fu
  :ensure t
  :bind (("s-z" . undo-fu-only-undo)
         ("s-Z" . undo-fu-only-redo)))

(use-package vundo
  :ensure t
  :bind ("s-u" . vundo)
  :config (set-face-attribute 'vundo-default nil :family "PlemolJP"))

(use-package ace-window
  :ensure t
  :bind ("C-o" . ace-window)
  :config
  (setq aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:height 3.0 :foreground "red" :weight bold))))))

(use-package winner :ensure nil :config (winner-mode 1))
(use-package beacon :ensure t :init (beacon-mode 1))

;; Mac標準ショートカットと行操作
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-w") 'kill-current-buffer)
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "s-t") #'tab-new)
(global-set-key (kbd "s-{") #'tab-previous)
(global-set-key (kbd "s-}") #'tab-next)

;; Command + 矢印キー でウィンドウ間を移動
(define-key global-map (kbd "s-<left>")  'windmove-left)
(define-key global-map (kbd "s-<right>") 'windmove-right)
(define-key global-map (kbd "s-<up>")    'windmove-up)
(define-key global-map (kbd "s-<down>")  'windmove-down)

(defun my/copy-line-or-region ()
  (interactive)
  (if (region-active-p) (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-end-position))))
(global-set-key (kbd "s-c") 'my/copy-line-or-region)

(provide 'init-ui)
;;; init-ui.el ends here
