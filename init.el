;; elpaca の初期化
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))


;;-----他のファイルの読み込み-----
(add-to-list 'load-path "~/.emacs.d/my-lisp")
(require 'coding)
(require 'org-related)
(require 'ui-completion)


;; org-modeの後にPandocの設定を
(use-package ox-pandoc
  :ensure t
  :after org
  :config
  (setq org-pandoc-options '((standalone . t)))
  (add-to-list 'org-export-backends 'pandoc))



(use-package transient
  :ensure t)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))  


(use-package treemacs
  :ensure t
  :bind (("C-x t t" . treemacs)
         ("M-0" . treemacs-select-window))  ;; Treemacsウィンドウにフォーカス
  :config
  (setq treemacs-width 35
        treemacs-show-hidden-files t
        treemacs-is-never-other-window t))

(add-hook 'treemacs-mode-hook
          (lambda () (display-line-numbers-mode -1)))


(use-package treemacs-nerd-icons
  :after treemacs
  :ensure t
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)



;; 端末関連
(use-package vterm
  :ensure t
  :commands vterm
  :config
  (setq vterm-shell "/bin/zsh")) ;; あなたのシェルに合わせて


;; まとめて行番号を非表示にする
(dolist (mode '(vterm-mode
                term-mode
                shell-mode
                eshell-mode))
  (add-hook (intern (format "%s-hook" mode))
            (lambda () (display-line-numbers-mode -1))))



;; ショートカット関連
(global-set-key (kbd "C-c t") #'eshell)


;; --- migemo 設定 ---
;;いつかやりたい。






;; バックアップや自動保存ファイルの作成方法を細かく制御
(setq backup-by-copying t    ; 上書きではなくコピーを保存
      delete-old-versions t  ; 古いバージョンを削除
      kept-new-versions 6    ; 新しいものを6世代保持
      kept-old-versions 2    ; 古いものを2世代保持
      version-control t)     ; バージョン管理を有効に


(use-package elfeed
  :ensure t
  :bind ("C-x w" . elfeed))
