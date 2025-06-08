;; ui-completion.el
;; 基本的な設定を中心に




;;;---日本語環境のセットアップ
;; 言語環境を日本語に設定
(set-language-environment "Japanese")
;; デフォルトの文字コードをUTF-8に設定
(prefer-coding-system 'utf-8)

;; デフォルトフォントを PlemolJP に設定
(set-face-attribute 'default nil
                    :family "PlemolJP"
                    :height 160) ;; 適宜調整

(defun my-org-mode-font-setup ()
  "Org-mode の基本フォントをヒラギノ丸ゴシックにし、表 (org-table) のみ PlemolJP に設定する"
  ;; Org-mode の本文フォントをヒラギノ丸ゴシック
  (face-remap-add-relative 'default :family "Hiragino Kaku Gothic Pro" :height 180)

  ;; org-table だけ PlemolJP に変更
  (set-face-attribute 'org-table nil :family "PlemolJP" :height 160)
  (set-face-attribute 'org-block nil :family "PlemolJP" :height 160)
  (set-face-attribute 'org-code nil :family "PlemolJP" :height 160)
  (set-face-attribute 'org-verbatim nil :family "PlemolJP" :height 160))

(add-hook 'org-mode-hook #'my-org-mode-font-setup)



;; 行番号表示
(global-display-line-numbers-mode t)
;; ツールバーの非表示
(tool-bar-mode -1)
;; スクロールバーの非表示
(scroll-bar-mode -1)

;; スクロールの改善?
(setq scroll-margin 3)
(setq scroll-conservatively 100)
(setq scroll-step 1)

(use-package good-scroll
  :ensure t
  :hook (after-init . good-scroll-mode))


;; フレームサイズの指定
(setq initial-frame-alist
      '((width	.	120)        ; 幅（文字単位）
        (height .	50)         ; 高さ（行単位）
        (top	.	50)         ; 画面上端からの距離（ピクセル）
        (right	.	100)))      ; 画面右端からの距離（ピクセル）


;;タブの有効化
(setq tab-bar-show t) ;; 1なら2個以上タブがあるときだけ表示、tなら常に表示
(tab-bar-mode 1)
(setq tab-bar-new-tab-choice "*dashboard*") ;; 新しいタブで dashboard を開く
;; scratchに変える事もできる

;; キーバインド
(global-set-key (kbd "C-x t n") #'tab-new)
(global-set-key (kbd "C-x t 0") #'tab-close)
(global-set-key (kbd "C-x t o") #'tab-next)
(global-set-key (kbd "C-x t p") #'tab-previous)

;; タブ名をバッファから推定（optional）
(setq tab-bar-tab-name-function #'tab-bar-tab-name-current)


(setq default-frame-alist
      '((width . 120)   ; 幅（文字単位）
        (height . 50))) ; 高さ（行単位）

(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-maris-dark t))
;; ef-maris-dark 結構良い。
;; ef-night も良さそうだな
;; ef-bio


(use-package spacemacs-theme
  :ensure t)

(use-package doom-themes
   :ensure t)


(use-package recentf
  :demand t
  :config
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 50)
  (add-hook 'kill-emacs-hook #'recentf-save-list)
  :bind
  (("C-c w r" . recentf-open)))

(use-package savehist
  :init
  (savehist-mode))

;; バックアップファイルを一箇所にまとめる
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "backups/" user-emacs-directory))))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-saves/" user-emacs-directory) t)))


(use-package nyan-mode
  :ensure t
  :config
  
  (setq nyan-animate-nyancat t      ;; Nyan catを動かす
        nyan-wavy-trail t)          ;; 虹の尾を波打たせる
  (nyan-mode 1)         
  (nyan-start-animation))


(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 2)
  (doom-modeline-icon t)             ;; アイコン表示（nerd-icons 使用）
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-nyan-mode t)       ;; ← Nyan Cat 表示
  :config
  (doom-modeline-mode 1))


(use-package consult
  :ensure t
  :bind
  (("C-s"   . consult-line)        ;; バッファ内検索
   ("C-x b" . consult-buffer)      ;; バッファ切り替え強化
   ("M-g i" . consult-imenu)       ;; 現バッファ内の関数や見出しへ
   ("s-r"   . consult-recent-file))) ;; recentf 強化

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.1))



(use-package olivetti
  :ensure t
  :hook (org-mode . olivetti-mode)
  :custom
  (olivetti-body-width 80)  ;; 本文幅を80文字に制限（任意で調整）
  (olivetti-minimum-body-width 60)
  (olivetti-style 'fringe)) ;; 左右余白をfringeで確保（visual-lineと相性良い）

(add-hook 'org-mode-hook #'olivetti-mode)
(add-hook 'org-mode-hook #'visual-line-mode)

;; Spacious padding
(use-package spacious-padding
  :ensure t
  :config
  (setq spacious-padding-widths
        '((internal-border-width . 4)  ;; フレームの内側に少し余白を
          (mode-line-width . 0)))       ;; モードラインの上下余白を少しだけ
  ;; 起動時に spacious-padding-mode を有効化
  (spacious-padding-mode 1))





(use-package dashboard
  :ensure t
  :init
  ;; Emacs起動時に dashboard を表示
  (setq initial-buffer-choice #'dashboard-open)

  ;; 表示する項目の数と種類
  (setq dashboard-items '((recents  . 10)
                          (bookmarks . 5)
                          (projects . 5)))

  ;; センタリングやアイコン表示など
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t)
  (setq dashboard-startup-banner 'official) ;; Emacs公式ロゴ

  :config
  (dashboard-setup-startup-hook))


(use-package vertico
  :ensure t
  :init
  (vertico-mode 1)
;;  :bind (:map vertico-map
;;              ("DEL" . vertico-directory-delete-word))   ;; デリートで固まりで削除
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))



(use-package pdf-tools
  :ensure t
  :magic ("%PDF" . pdf-view-mode)
  :config
  ;; 自動ビルド＆インストール（初回のみ時間がかかる）
  (pdf-tools-install)

  ;; デフォルトで開いたら自動的に「表示専用」にする
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)
  (define-key pdf-view-mode-map (kbd "n") 'pdf-view-next-page)
  (define-key pdf-view-mode-map (kbd "p") 'pdf-view-previous-page))




(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (setq embark-indicators '(embark-minimal-indicator)))

(use-package embark-consult
  :after (embark consult)
  :ensure t)

(provide 'ui-completion)
