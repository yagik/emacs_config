;;; early-init.el --- 起動の最初期に読み込まれる設定 -*- lexical-binding: t -*-
;;
;; Emacs 27 以降、init.el より前・最初のフレーム（ウィンドウ）を描く前に読まれる。
;; ここには「起動速度」と「ウィンドウの見た目のちらつき防止」に関わる設定だけを置く。
;; それ以外の設定は config.org に書く。

;; --- 1. 起動中はガベージコレクション（GC）をほぼ止めて高速化 ---
;;     起動が終わったら通常の値に戻す（戻さないと動作中にカクつく原因になる）
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 16 1024 1024)))) ; 16MB

;; --- 2. 起動中はファイル名ハンドラを外して、ファイル読み込みを軽くする ---
(defvar my/file-name-handler-alist-saved file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda () (setq file-name-handler-alist my/file-name-handler-alist-saved)))

;; --- 3. ネイティブコンパイルの警告を表示しない ---
(setq native-comp-async-report-warnings-errors 'silent)

;; --- 4. ウィンドウの大きさ・位置と、ツールバー／スクロールバー非表示 ---
;;     フレームを描く前に指定するので、起動時に一瞬ツールバーが見える「ちらつき」がなくなる
(setq initial-frame-alist '((width . 120) (height . 50) (top . 50) (left . 100)))
(setq default-frame-alist '((width . 120) (height . 50)
                            (tool-bar-lines . 0)
                            (vertical-scroll-bars . nil)))
(setq inhibit-startup-message t)

;;; early-init.el ends here
