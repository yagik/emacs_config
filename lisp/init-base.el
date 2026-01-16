;;; init-base.el --- Basic settings -*- lexical-binding: t -*-

;; 起動高速化とMac最適化
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

;; カッコの閉じ忘れ・視認性対策
(electric-pair-mode 1)   ; カッコを自動で閉じる
(show-paren-mode 1)      ; 対応するカッコを光らせる
(setq show-paren-delay 0)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; 3. パッケージ管理 (標準 package.el)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)

;; 4. フォントと外見の基本設定
(defun my/setup-fonts ()
  (interactive)
  (when (member "PlemolJP" (font-family-list))
    (set-face-attribute 'default nil :family "PlemolJP" :height 170)))
(my/setup-fonts)

;; UIのノイズをカット
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)


;; 行番号表示に関する設定
(global-display-line-numbers-mode t)

;; 2. 行番号のタイプ設定
;; 't (絶対行) が一般的ですが、'relative (相対行) も根強い人気があります
(setq display-line-numbers-type t)

;; 3. 桁数が増えても横にガタつかないようにする（必須設定）
(setq display-line-numbers-width-start t)

;; 4. 【おすすめ】行番号のフォントだけ固定幅にする
;; プロポーショナルフォントを全体で使っていても、行番号のズレを防げます
(set-face-attribute 'line-number nil :inherit 'fixed-pitch)

;; 5. 特定のモード（ミニバッファやヘルプなど）では表示しない
(dolist (hook '(help-mode-hook
                info-mode-hook
                custom-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode -1))))








;; スクロールを滑らかにする (Emacs 29以降の標準機能)
(pixel-scroll-precision-mode 1)

;; 日本語入力中の挙動を安定させる
(setq redisplay-dont-pause t)


;; 防御的設定 (ファイル管理)
(let ((backup-dir (expand-file-name "backups/" user-emacs-directory))
      (auto-save-dir (expand-file-name "auto-saves/" user-emacs-directory)))
  (unless (file-exists-p backup-dir) (make-directory backup-dir t))
  (unless (file-exists-p auto-save-dir) (make-directory auto-save-dir t))
  (setq backup-directory-alist `((".*" . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-save-dir t))
        create-lockfiles nil))

(provide 'init-base)
;;; init-base.el ends here
