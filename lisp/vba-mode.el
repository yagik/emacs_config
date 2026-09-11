;;; vba-mode.el --- Final Polish VBA major mode -*- lexical-binding: t; -*-

(require 'prog-mode)

;; -------------------------------------------------------------------------
;; 1. Customization
;; -------------------------------------------------------------------------

(defgroup vba-mode nil
  "Major mode for editing Visual Basic for Applications."
  :group 'languages)

(defcustom vba-mode-indent-offset 4
  "Indentation amount per level."
  :type 'integer
  :safe #'integerp
  :group 'vba-mode)

(defcustom vba-mode-continuation-offset 4
  "Extra indentation for line continuations (lines ending in _).
Set to 0 to align with the previous line."
  :type 'integer
  :safe #'integerp
  :group 'vba-mode)

(defcustom vba-mode-indent-case-extra-offset 0
  "Indentation of `Case` lines relative to the `Select Case` line.
- 0: align `Case` with `Select Case` (Standard)
- 4: indent `Case` lines one level deeper than `Select Case`"
  :type 'integer
  :safe #'integerp
  :group 'vba-mode)

;; -------------------------------------------------------------------------
;; 2. Syntax & Highlights
;; -------------------------------------------------------------------------

(defvar vba-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?' "<" st)   ; Comment start
    (modify-syntax-entry ?\n ">" st)  ; Comment end
    (modify-syntax-entry ?\" "\"" st) ; String
    (modify-syntax-entry ?_ "w" st)   ; Symbol constituent
    (modify-syntax-entry ?: "." st)   ; Punctuation separator
    st)
  "Syntax table for `vba-mode'.")

(defconst vba-mode-keywords
  (regexp-opt '("Option" "Explicit" "Sub" "Function" "Property" "End"
                "If" "Then" "Else" "ElseIf" "Select" "Case" "Default"
                "For" "Each" "Next" "Do" "Loop" "While" "Wend" "Until"
                "With" "Exit" "GoTo" "On" "Error" "Resume"
                "Dim" "ReDim" "Preserve" "Public" "Private" "Static" "Friend"
                "As" "Set" "New" "Call" "Let" "Get"
                "ByVal" "ByRef" "Optional" "ParamArray"
                "And" "Or" "Not" "Xor" "Mod" "Is" "Like" "Imp" "Eqv") 'symbols))

(defconst vba-mode-types
  (regexp-opt '("Boolean" "Byte" "Integer" "Long" "LongLong" "Single" "Double"
                "Currency" "String" "Date" "Variant" "Object" "Collection") 'symbols))

;; Minimized Built-ins to reduce false positives (Removed 'Events')
(defconst vba-mode-builtins
  (regexp-opt '("Application" "Debug" "Err" "VBA" "Me") 'symbols))

(defconst vba-mode-constants
  (regexp-opt '("True" "False" "Nothing" "Empty" "Null" "vbCr" "vbLf" "vbCrLf"
                "vbNullString" "vbTab" "vbOK" "vbCancel" "vbYes" "vbNo") 'symbols))

(defvar vba-mode-font-lock-keywords
  `((,vba-mode-keywords . font-lock-keyword-face)
    (,vba-mode-types . font-lock-type-face)
    (,vba-mode-builtins . font-lock-builtin-face)
    (,vba-mode-constants . font-lock-constant-face)
    ;; Rem comments (Must be followed by space or EOL)
    ("^[ \t]*\\(Rem\\b\\(?:[ \t].*\\)?\\)$" 1 font-lock-comment-face)
    ;; Function names
    ("\\_<\\(?:Sub\\|Function\\|Property\\)\\_>[ \t]+\\([A-Za-z0-9_]+\\)"
     1 font-lock-function-name-face)
    ;; Variable declarations
    ("\\_<\\(?:Dim\\|Const\\|Public\\|Private\\)\\_>[ \t]+\\([A-Za-z0-9_]+\\)"
     1 font-lock-variable-name-face)
    ;; Labels
    ("^[ \t]*\\([A-Za-z0-9_]+\\):" 1 font-lock-constant-face)))

;; -------------------------------------------------------------------------
;; 3. Indentation Helpers (Predicates)
;; -------------------------------------------------------------------------

(defun vba-mode--prev-meaningful-line ()
  "Return the point at the beginning of the previous non-blank/non-comment line."
  (save-excursion
    (let ((found nil))
      (while (and (not found) (zerop (forward-line -1)))
        (unless (looking-at-p "^[ \t]*\\(?:'.*\\)?$")
          (setq found (point))))
      found)))

(defun vba-mode--line-str-at (pt)
  (save-excursion
    (goto-char pt)
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun vba-mode--curr-line-str ()
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun vba-mode--select-case-p (s)
  (string-match-p "^[ \t]*Select\\s-+Case\\b" s))

(defun vba-mode--case-line-p (s)
  (string-match-p "^[ \t]*Case\\b" s))

(defun vba-mode--if-block-opener-p (s)
  ;; Matches "If ... Then" ONLY if Then is at EOL or followed by comment
  (string-match-p "If\\b.*\\bThen\\s-*\\(?:'\\|$\\)" s))

(defun vba-mode--continuation-p (s)
  (string-match-p ".*_[ \t]*\\(?:'.*\\)?$" s))

(defun vba-mode--other-opener-p (s)
  ;; Note: 'Case' is purposely excluded here. It is handled in Step 4.
  (string-match-p
   (concat "^[ \t]*\\(?:"
           "For\\b"
           "\\|Do\\b"
           "\\|With\\b"
           "\\|While\\b"
           "\\|Sub\\b"
           "\\|Function\\b"
           "\\|Property\\b"
           "\\|Type\\b"
           "\\|Enum\\b"
           "\\|Else\\b"
           "\\|ElseIf\\b"
           "\\)")
   s))

(defun vba-mode--closer-p (s)
  (string-match-p
   (concat "^[ \t]*\\(?:"
           "End\\s-+\\(?:If\\|Sub\\|Function\\|Property\\|Select\\|With\\|Type\\|Enum\\)\\b"
           "\\|Next\\b"
           "\\|Loop\\b"
           "\\|Wend\\b"
           "\\|Else\\b"
           "\\|ElseIf\\b"
           "\\)")
   s))

;; -------------------------------------------------------------------------
;; 4. Indentation Logic (Robust 4-Step Process)
;; -------------------------------------------------------------------------

(defun vba-mode--calc-indent ()
  "Calculate indentation level using a 4-step state machine."
  (let ((indent 0)
        (prev-point (vba-mode--prev-meaningful-line))
        (case-fold-search t))

    (if (not prev-point)
        0
      (let* ((prev-str (vba-mode--line-str-at prev-point))
             (prev-indent (save-excursion (goto-char prev-point) (current-indentation)))
             (curr-str (vba-mode--curr-line-str)))

        ;; Base: previous meaningful line indentation
        (setq indent prev-indent)

        ;; --- Step 1: Continuation from previous line ---
        (when (vba-mode--continuation-p prev-str)
          (setq indent (+ indent vba-mode-continuation-offset)))

        ;; --- Step 2: Opener effects (Block start) ---
        (cond
         ((vba-mode--if-block-opener-p prev-str)
          (setq indent (+ indent vba-mode-indent-offset)))
         ((vba-mode--select-case-p prev-str)
          (setq indent (+ indent vba-mode-indent-offset)))
         ((vba-mode--other-opener-p prev-str)
          (setq indent (+ indent vba-mode-indent-offset))))

        ;; --- Step 3: Current line adjustments (Dedent/Alignment) ---
        (cond
         ;; Case line: Align relative to Select Case (or prev level)
         ((vba-mode--case-line-p curr-str)
          (if (vba-mode--select-case-p prev-str)
              ;; If following Select Case directly -> Align + extra offset
              (setq indent (+ prev-indent vba-mode-indent-case-extra-offset))
            ;; Otherwise -> Dedent 1 level (undoing Case Content indent) + extra offset
            (setq indent (+ (max 0 (- indent vba-mode-indent-offset))
                            vba-mode-indent-case-extra-offset))))

         ;; Standard Closer: Dedent 1 level
         ((vba-mode--closer-p curr-str)
          (setq indent (max 0 (- indent vba-mode-indent-offset)))))

        ;; --- Step 4: Case Content Indentation ---
        ;; If prev was Case, and current is NOT Case (content), add indent.
        ;; We trust prev-indent here (standard Emacs practice).
        ;; If prev line was indented correctly, (prev-indent + offset) is correct.
        (when (and (vba-mode--case-line-p prev-str)
                   (not (vba-mode--case-line-p curr-str)))
          (setq indent (+ prev-indent vba-mode-indent-offset)))

        (max 0 indent)))))

(defun vba-mode-indent-line ()
  (interactive)
  (let ((indent (vba-mode--calc-indent))
        (pos (- (point-max) (point))))
    (indent-line-to indent)
    (if (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos)))))

;; -------------------------------------------------------------------------
;; 5. Mode Definition & Autoloads
;; -------------------------------------------------------------------------

;;;###autoload
(define-derived-mode vba-mode prog-mode "VBA"
  "Major mode for editing Excel VBA."
  :syntax-table vba-mode-syntax-table
  (setq-local font-lock-defaults '(vba-mode-font-lock-keywords nil t))
  (setq-local indent-line-function #'vba-mode-indent-line)
  (setq-local comment-start "' ")
  (setq-local comment-start-skip "'+\\s-*")
  (setq-local outline-regexp "^[ \t]*\\(Sub\\|Function\\|Property\\|Enum\\|Type\\)")
  (setq-local case-fold-search t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(?:bas\\|cls\\|frm\\|vbs\\)\\'" . vba-mode))

(with-eval-after-load 'org
  ;; Support both lowercase and uppercase (VBA) for convenience
  (dolist (lang '("vba" "VBA" "vbs"))
    (add-to-list 'org-src-lang-modes (cons lang 'vba))))

(provide 'vba-mode)
;;; vba-mode.el ends here
