;;; --- 1. 起動高速化とMac最適化 ---
(setq gc-cons-threshold (* 100 1024 1024))
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold (* 2 1024 1024))))

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; ウインドウサイズと位置 (M4 MacBook Pro用)
(setq initial-frame-alist '((width . 120) (height . 50) (top . 50) (left . 100)))
(setq default-frame-alist '((width . 120) (height . 50)))

;; Macのキー設定
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

;; Homebrewのパスを通す
(when (eq system-type 'darwin)
  (setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH")))
  (add-to-list 'exec-path "/opt/homebrew/bin"))

;;; --- 2. カッコの閉じ忘れ・視認性対策 ---
(electric-pair-mode 1)   ; カッコを自動で閉じる
(show-paren-mode 1)      ; 対応するカッコを光らせる
(setq show-paren-delay 0)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;;; --- 3. パッケージ管理 (標準 package.el) ---
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)

;;; --- 4. フォントと外見の設定 ---
(defun my/setup-fonts ()
  (interactive)
  (when (member "PlemolJP" (font-family-list))
    (set-face-attribute 'default nil :family "PlemolJP" :height 170)))
(my/setup-fonts)

(defun my/org-mode-visual-settings ()
  "Org-modeの本文をヒラギノに、表などを等幅にする"
  (face-remap-add-relative 'default :family "Hiragino Kaku Gothic Pro" :height 180)
  (set-face-attribute 'org-table nil :family "PlemolJP" :height 160)
  (set-face-attribute 'org-block nil :family "PlemolJP" :height 160)
  (set-face-attribute 'org-code nil :family "PlemolJP" :height 160)
  (set-face-attribute 'org-verbatim nil :family "PlemolJP" :height 160)
  (setq-local line-spacing 0.2))

(use-package catppuccin-theme
  :ensure t
  :config
  (setq catppuccin-flavor 'mocha)
  (load-theme 'catppuccin t))

;; UIのノイズをカット
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; --- 行番号表示の設定 ---
;; 全てのバッファでデフォルトで行番号を表示する
(global-display-line-numbers-mode t)

;; ただし、行番号が出ると不自然なモード（DashboardやTerminalなど）では無効化する
(dolist (mode '(dashboard-mode-hook
                vterm-mode-hook
                pdf-view-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1))))

;; 行番号の幅を少しゆったりさせる（お好みで）
(setq-default display-line-numbers-width 3)

;;; --- 5. モダンUI & 補完 (Vertico, Consult) ---
(use-package vertico :init (vertico-mode 1))
(use-package marginalia :init (marginalia-mode 1))
(use-package orderless :custom (completion-styles '(orderless basic)))

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("s-r"   . consult-recent-file)
         ("C-s"   . consult-line)
         ("s-f"   . consult-line))
  :config (setq consult-line-numbers-widen t))

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

;;; --- 6. 編集効率化とace-window ---
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

;; 追加：Command + 矢印キー でウィンドウ間を移動
(define-key global-map (kbd "s-<left>")  'windmove-left)
(define-key global-map (kbd "s-<right>") 'windmove-right)
(define-key global-map (kbd "s-<up>")    'windmove-up)
(define-key global-map (kbd "s-<down>")  'windmove-down)

(defun my/copy-line-or-region ()
  (interactive)
  (if (region-active-p) (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-end-position))))
(global-set-key (kbd "s-c") 'my/copy-line-or-region)

;;; --- 7. 日本語検索 (Migemo) ---
(use-package migemo
  :if (executable-find "cmigemo")
  :config
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/opt/homebrew/share/migemo/utf-8/migemo-dict")
  (migemo-init))

;;; --- 8. Org-mode & PDF (メイン執筆環境) ---
(use-package org
  :hook (org-mode . my/org-mode-visual-settings)
  :custom
  (org-startup-indented t)
  (org-hide-emphasis-markers t))

(use-package olivetti
  :hook (org-mode . olivetti-mode)
  :custom (olivetti-body-width 90))


(use-package pdf-tools
  :ensure t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-loader-install)
  
  ;; --- [1] hjkl での移動設定 ---
  (with-eval-after-load 'pdf-view
    (define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
    (define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)
    (define-key pdf-view-mode-map (kbd "l") 'image-forward-hscroll)
    (define-key pdf-view-mode-map (kbd "h") 'image-backward-hscroll))

  ;; (a) Retinaディスプレイ(M4 Mac)への最適化
  ;; 高解像度でのレンダリングをスムーズにします
  (setq pdf-view-use-scaling t)
  (setq-default pdf-view-display-size 'fit-width)

  ;; (c) 検索結果を一覧表示する (pdf-occur)
  ;; PDF内でキーワード検索をした際、該当箇所を別バッファにリストアップします
  (define-key pdf-view-mode-map (kbd "o") 'pdf-occur)

  ;; (d) マウス・トラックパッドでのスムーズなスクロール
  (setq pdf-view-continuous t)

  ;; (e) ページ番号の表示
  (add-hook 'pdf-view-mode-hook (lambda () (line-number-mode -1))))


(use-package org-download
  :after org
  :config (org-download-enable))

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :custom (org-modern-table nil)) ; 表のズレ防止

(use-package denote
  :hook (dired-mode . denote-dired-mode) ; ファイル一覧画面でもDenote名が見やすく
  :custom
  (denote-directory "~/Documents/denote")
  (denote-file-type 'org)
  :config (denote-rename-buffer-mode 1)
  :bind (("C-c n n" . denote)
         ("C-c n f" . denote-open-or-create)))

;; -------------------------------------------------------------------------
;; Org-mode と PDF Tools の連携 (リンク機能)
;; -------------------------------------------------------------------------

;; 1. 【重要】「ファイルを開く」操作でもルールを適用させるスイッチ
;;    これがないと、設定したルールが無視されることがあります。
(setq switch-to-buffer-obey-display-actions t)

;; 2. 【新アプローチ】PDFは「右側のサイドウィンドウ」として開く
;;    「窓を割る」のではなく「右側に専用エリアを確保しろ」という命令です。
;;    これならフォントサイズ等の影響を受けず、強制的に右に出ます。
(add-to-list 'display-buffer-alist
             '("\\.pdf\\'"
               (display-buffer-in-side-window)  ; サイドウィンドウを使う
               (side . right)                   ; 右側に配置
               (slot . 0)
               (window-width . 0.5)             ; 画面の50%を使う
               (preserve-size . (t . t))        ; 勝手にサイズを変えさせない
               ))

(use-package org-pdftools
  :ensure t
  :hook (org-mode . org-pdftools-setup-link)
  :config
  (org-pdftools-setup-link)
  ;; 以前のような関数による上書きは不要です。
  ;; Emacs標準のルール(display-buffer-alist)に任せるのが最も確実です。
  )

;;; --- 9. 防御的設定 (ファイル管理) ---
(let ((backup-dir (expand-file-name "backups/" user-emacs-directory))
      (auto-save-dir (expand-file-name "auto-saves/" user-emacs-directory)))
  (unless (file-exists-p backup-dir) (make-directory backup-dir t))
  (unless (file-exists-p auto-save-dir) (make-directory auto-save-dir t))
  (setq backup-directory-alist `((".*" . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-save-dir t))
        create-lockfiles nil))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))


;; ==========================================================
;; Phase 3: プログラミング・執筆の基盤 (LSP & Treesit)
;; ==========================================================


;;; Git連携 (Magit) - 履歴管理の魔法
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)) ; C-x g でGitの状態を表示
  :config
  ;; コミットメッセージ入力時に自動的に日本語入力をONにする設定（将来用）
  ;; (add-hook 'git-commit-mode-hook (lambda () (set-input-method "japanese-skk")))
  )

;; 1. 言語構造の深い理解 (Tree-sitter)
;; Emacs 29からの新機能。従来の正規表現による色付けより正確で高速です。

(use-package treesit
  :ensure nil
  :if (boundp 'treesit-font-lock-level)
  :config
  (setq treesit-font-lock-level 4))


;; 2. モダンな補完・診断 (Eglot)
;; Emacs標準のLSPクライアント。設定不要で、エラー箇所に下線を引いたり補完を助けます。
(use-package eglot
  :ensure nil ; 標準機能
  :hook
  ;; 各言語の「ts（Tree-sitter）モード」で自動起動するように設定
  ((python-ts-mode . eglot-ensure)
   (js-ts-mode . eglot-ensure))
  :config
  ;; Command + . で修正案を表示 (macOSの操作感に合わせる)
  (keymap-set eglot-mode-map "s-." 'eglot-code-actions))

;; 3. 識別子の強調表示 (symbol-overlay)
;; カーソル下の変数名などと同じ名前のものを一斉にハイライトします。
;; 論文執筆中に「同じ用語をどこで使ったか」探すのにも便利です。
(use-package symbol-overlay
  :ensure t
  :bind ("M-i" . symbol-overlay-put)) ; Alt + i でハイライトのON/OFF
