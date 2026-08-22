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


(use-package which-key
  :ensure nil  ; ★Emacs 30では組み込みなので、ダウンロード不要
  :init
  (which-key-mode 1)
  :config
  ;; 0.5秒待ってからガイドを表示（一瞬で出るとチカチカするので、このくらいが快適です）
  (setq which-key-idle-delay 0.05)
  ;; ミニバッファの横に余裕を持って表示
  (setq which-key-side-window-location 'bottom))


;; モダンUI & 補完 (Vertico, Consult)
(use-package vertico
  :init
  (vertico-mode 1)
  :config
  ;; 履歴（選んだ回数や新しさ）を最優先し、
  ;; それ以外はアルファベット順に並べる「賢いソート」を有効にします。
  (setq vertico-sort-function #'vertico-sort-history-alpha))

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


;; 選択範囲を「意味のまとまり」で段階的に広げる
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)   ; 候補1：Control + =
         ("s-e" . er/expand-region)   ; 候補2：Command + e (Expand)
         ("C--" . er/contract-region) ; 狭める方は Control + -
         ("s-E" . er/contract-region))) ; 狭める方は Command + Shift + e



;; -----------------------------------------------------------------------------
;; Google日本語(日：青) + Mac標準ABC(英：橙) の自動切替
;; -----------------------------------------------------------------------------
(use-package sis
  :ensure t
  :config
  (sis-ism-lazyman-config
   "com.apple.keylayout.US"
   "com.google.inputmethod.Japanese.base")

  ;; ★ここが本命：リーダーキーを「prefix override」に追加
  (add-to-list 'sis-prefix-override-keys "C-;")

  ;; prefix key を英語に落とす機能は respect-mode が担当
  (sis-global-respect-mode t)

  ;; （任意）カーソル色
  (setq sis-cursor-color-indicator '("#FF9500" "#4285F4" "red"))
  (sis-global-cursor-color-mode t))

;;---------
;; 英語関係
;;---------
;
;; --- osx-dictionary (単語検索用) ---
(use-package osx-dictionary
  :ensure t
  :bind (("C-c d" . osx-dictionary-search-pointer)    ; カーソル下の単語を検索
         ("C-c i" . osx-dictionary-search-input)))    ; 入力して検索

;; ;; --- google-translate (文章翻訳用) ---
;; ;; --- 最近はgo-translateが主流と
;; (use-package google-translate
;;   :ensure t
;;   :bind (("C-c t" . google-translate-at-point)        ; カーソル下の単語/選択範囲を翻訳
;;          ("C-c T" . google-translate-query-translate)); 入力して翻訳
;;   :config
;;   ;; 日本語への翻訳をデフォルトにする設定
;;   (setq google-translate-default-source-language "en")
;;   (setq google-translate-default-target-language "ja"))

;; (use-package gt
;;   :ensure t
;;   :bind (("C-c t" . gt-do-translate)) ; 翻訳実行
;;   :config
;;   ;; デフォルトの翻訳設定を定義
;;   (setq gt-default-translator
;;         (gt-translator
;;          ;; 翻訳の向き（自動判別しつつ日英・英日を切り替え）
;;          :taker   (gt-taker :langs '(en ja) :prompt t)
;;          ;; エンジン（まずはGoogle翻訳。DeepL等も追加可能）
;;          :engines (list (gt-google-engine))
;;          ;; 出力先（バッファに表示。ポップアップが好きなら gt-posframe-render も可）
;;          :render  (gt-buffer-render))))

(use-package posframe
  :ensure t
  :demand t)

(use-package gt
  :ensure t
  :bind (("C-c t" . gt-translate))
  :config
  (setq gt-langs '(en ja))
  (require 'gt-render-posframe)
  (require 'gt-engine-google)

  (setq gt-default-translator
        (gt-translator
         ;; :text 'buffer と :pick 'paragraph の組み合わせで
         ;; 「選択範囲があればそれ、なければ段落」を自動取得します
         :taker   (gt-taker :text 'buffer :pick 'region-or-paragraph :prompt 'disable)
         :engines (list (gt-google-engine))
         :render  (gt-posframe-render))))


;;----
;; カーソル位置を光らせる（ジャンプ後の見失い防止）
(use-package beacon
  :ensure t
  :init (beacon-mode 1)
  :custom (beacon-color "#ce7e8e"))
;; Diredモードの時はbeacon（光るエフェクト）をオフにする
(add-hook 'dired-mode-hook (lambda () (beacon-mode -1)))


(provide 'init-ui)
;;; init-ui.el ends here
