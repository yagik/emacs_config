;; Org-mode関連の設定
;; org-roam, denote, howm 関係もここに

;; pandoc 関係は writing系にまとめる


(use-package org
  :custom
  (org-startup-indented t)
  (org-indent-indentation-per-level 1)   ;; インデント幅を最小限 デフォルト2
  ;;(org-adapt-indentation nil)            ;; 内容はインデントしない
  (org-hide-emphasis-markers t)
  (org-startup-with-inline-images t)
  (org-image-actual-width '(450))
  (org-pretty-entities t)
  (org-use-sub-superscripts "{}")
  (org-id-link-to-org-use-id t)
  (org-support-shift-select t)
  (org-fold-catch-invisible-edits 'show))

(setq-local line-spacing 0.2)  ;; 行間の設定
(setq org-return-follows-link t)

;; Orgリンクで新しいウィンドウを使わない（fileリンクなど）
(setq org-link-frame-setup
      '((vm . vm-visit-folder)
        (vm-imap . vm-visit-imap-folder)
        (gnus . gnus)
        (file . find-file))) ;; ← 分割を抑制


(with-eval-after-load 'org
  (setq org-attach-id-dir ".attach/")
  (setq org-attach-use-inheritance t))

;; Org-modeでだけ行番号を非表示にする
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)  ;; v2以降の確認をスキップ
  :custom
  (org-roam-directory (file-truename "~/Documents/org-roam"))  ;; Roamノートの保存場所
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n a" . org-roam-alias-add))
  :config
  (org-roam-db-autosync-mode)  ;; v2.1以降推奨の自動DB同期
 ;; リンクを同じウィンドウで開く設定
  (setq org-roam-node-visit-trampolines nil)
  (setq org-roam-node-visit-other-window nil))
(setq org-return-follows-link t)


(use-package org-roam-ui
  :ensure t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil)
  :hook (after-init . org-roam-ui-mode)
  :config
  ;; native-compile 時の関数未定義エラーを防ぐ
  (require 'org-roam-ui))


(use-package org-roam-bibtex
  :ensure t
  :after org-roam)

(use-package consult-org-roam
  :after org-roam
  :ensure t
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep) ;; ripgrep を使用（推奨）
  (consult-org-roam-buffer-names '("*scratch*" "*Messages*"))
  (consult-org-roam-file-ignore-regexp "\\.git/")
  :config
  (consult-org-roam-mode 1))


;; denote関連
(use-package denote
  :ensure t
  :config
  ;; ノートの保存ディレクトリを指定
  (setq denote-directory "~/Documents/notes")

  ;; よく使うキーワード（タグ）を定義（必要に応じて追加OK）
  (setq denote-known-keywords '("work" "research" "personal" "idea" "todo"))

  ;; タイトルからタグを自動推定＆整列
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)

  ;; Orgファイル形式をデフォルトに
  (setq denote-file-type 'org)

  ;; フルパスではなく相対リンクを使う（可搬性↑）
  (setq denote-link-backends '(denote-org-link)))

(global-set-key (kbd "C-c n n") #'denote)                 ;; 新規ノート
(global-set-key (kbd "C-c n d") #'denote-date)            ;; 日付付きノート
(global-set-key (kbd "C-c n f") #'denote-open-or-create)  ;; ノート検索 or 作成
(global-set-key (kbd "C-c n l") #'denote-link)            ;; ノート間リンクを作成
(global-set-key (kbd "C-c n b") #'denote-backlinks)       ;; バックリンク一覧




(use-package org-appear
  :ensure t
  :hook
  (org-mode . org-appear-mode))


(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :custom
  ;; 見出しのブレット記号をSpacemacs風に
  (org-superstar-headline-bullets-list '("◉" "○" "◆" "❖" "✸" "▸"))

  ;; 見出しの `*` を消す
  (org-superstar-remove-leading-stars t)

  ;; `*` をインデントに置き換える
  (org-superstar-leading-bullet ?\s)

  ;; 箇条書き記号も変更（任意）
  (org-superstar-item-bullet-alist '((?+ . ?•) (?- . ?–)))

  ;; `#+BEGIN_SRC` などの開始/終了行にも対応（必要なら）
  (org-superstar-special-todo-items t))

;; org-modeのBulletの大きさを調整する。
(with-eval-after-load 'org
  (custom-set-faces
   '(org-level-1 ((t (:height 1.2 :weight bold))))
   '(org-level-2 ((t (:height 1.1 :weight normal))))
   '(org-level-3 ((t (:height 1.05 :weight normal))))
   '(org-level-4 ((t (:height 1.0 :weight normal))))
   '(org-level-5 ((t (:height 1.0 :weight normal))))
   '(org-level-6 ((t (:height 1.0 :weight normal))))
   '(org-level-7 ((t (:height 1.0 :weight normal))))
   '(org-level-8 ((t (:height 1.0 :weight normal))))))


(use-package org-download
  :ensure t
  :after org
  :hook (org-mode . org-download-enable)
  :config
  ;; ノート内の画像を保存するサブフォルダを設定
  (setq org-download-image-dir "img")

  ;; ヘッダの階層によって自動的にサブフォルダを分けない
  (setq org-download-heading-lvl nil)

  ;; クリップボード画像の貼り付けキー（macOSではcmd+v に割り当てる例）
  (define-key org-mode-map (kbd "s-v") #'org-download-clipboard))


;; journal機能はMonthlyにする。Denoteは使わない。
(defun my/open-monthly-journal ()
  "Open monthly journal file for the current month, creating it if needed."
  (interactive)
  (let* ((dir "~/Documents/notes/journal-monthly/")
         (filename (format-time-string "%Y-%m-journal.org"))
         (filepath (expand-file-name filename dir)))
    (unless (file-exists-p filepath)
      (make-directory dir t)
      (with-temp-buffer
        (insert (format "#+title: %s\n\n" (format-time-string "%Y年%m月の記録")))
        (write-file filepath)))
    (find-file filepath)))

(global-set-key (kbd "C-c n j") #'my/open-monthly-journal)


(defun my/denote-batch-rename ()
  "denote-directory 内の全ノートを front-matter に基づいて再命名する。"
  (interactive)
  (let ((files (denote-directory-files)))
    (dolist (file files)
      (with-current-buffer (find-file-noselect file)
        (let ((new-name (ignore-errors
                          (denote--rename-buffer-using-front-matter))))
          (when new-name
            (message "Renamed: %s → %s"
                     (file-name-nondirectory file)
                     (file-name-nondirectory new-name))))))))


(use-package consult-notes
  :ensure t
  :after denote
  :init
  (setq consult-notes-sources
        `(("Denote notes" ?d ,denote-directory)))

  ;; consult-notes の検索でタイトルにもslugにもマッチさせる
  (setq consult-notes-file-format-function #'consult-notes-file-format-denote)
  (setq consult-notes-denote-display-title-format 'title) ;; または 'full-title
  :config
  (consult-notes-denote-mode))  ;; ← denote モードとして動作


(defun consult-notes-search ()
  "Use `consult-grep` to search notes directory with better Japanese support."
  (interactive)
  (let ((default-directory (expand-file-name denote-directory)))
    (consult-grep)))


(global-set-key (kbd "C-c n s") #'consult-notes-search)  ;; ノート内検索（ripgrep）
(global-set-key (kbd "C-c n o") #'consult-notes)         ;; ノート一覧から選択・開く


(use-package howm
  :ensure t
  :init
  ;; 
  ;; Options: Remove the leading ";" in the following lines if you like.
  ;; 
  ;; Format
  ;(require 'howm-markdown) ;; Write notes in markdown-mode. (*1)
 (require 'howm-org) ;; Write notes in Org-mode. (*2)
  ;; 
  ;; Preferences
  (setq howm-directory "~/Documents/howm") ;; Where to store the files?
  (setq howm-follow-theme t) ;; Use your Emacs theme colors. (*3)
  ;; 
  ;; Performance
  (setq howm-menu-expiry-hours 1) ;; Cache menu N hours. (*4)
  (setq howm-menu-refresh-after-save nil) ;; Speed up note saving. (*5)

  (setq howm-view-title-header "#+TITLE: ")
  (setq howm-template "#+TITLE: \n\n* "))


(provide 'org-related)
