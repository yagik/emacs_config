(setq package-enable-at-startup nil)

(require 'subr-x)

(setenv "LIBRARY_PATH"
        (string-join
         '("/opt/homebrew/Cellar/gcc/15.1.0/lib"
           "/opt/homebrew/Cellar/libgccjit/15.1.0/lib/gcc/"
           "/opt/homebrew/Cellar/gcc/15.1.0/lib/gcc/current/gcc/aarch64-apple-darwin24/14")
         ":"))

(setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH")))
(add-to-list 'exec-path "/opt/homebrew/bin")


;; スタートアップメッセージを無効に
(setq inhibit-startup-message t)

;; 自動生成ファイルを無効に
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)
(setq create-lockfiles nil)


;; GC/Memory
(setq gc-cons-threshold (* 10 128 1024 1024))
(setq garbage-collection-messages nil)


;; デフォルトのpathをかえる
(setq default-directory "~/")
(setq command-line-default-directory "~/")

;; scratch buffer
(setq initial-scratch-message nil)
(setq initial-major-mode 'fundamental-mode)

;; エラーを抑制
;; (setq native-comp-async-report-warnings-errors nil)
