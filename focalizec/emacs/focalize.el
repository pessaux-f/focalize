;; *********************************************************************
;;
;;                        FoCaLiZe compiler
;;
;;            François Pessaux
;;
;;                 LIP6 - INRIA Rocquencourt - ENSTA ParisTech
;;
;;  Copyright 2007 - 2012 LIP6 and INRIA
;;            2012 ENSTA ParisTech
;;  Distributed only by permission.
;;
;; *********************************************************************

;; To use, add in your .emacs the following lines without the ";;":
;; (setq auto-mode-alist (cons '("\\.fcl$" . focalize-mode) auto-mode-alist))
;; (autoload 'focalize-mode "focalize" "Major mode for editing FoCaLiZe code." t)

(defvar focalize-mode-hook nil)


;; Define particular keys binding.
(defvar focalize-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap used for FoCaLize mode.")


;; Binds .flc files to focalize mode.
(add-to-list 'auto-mode-alist '("\\.fcl\\'" . focalize-mode))


;; Customizable variable specifying the number of spaces for indentation.
(defcustom focalize-indent-quantum 2
  "Indentation quantum in number of space characters."
  :type 'integer :group 'focalize)



(defun first-keyword-of-line ()
  ; Local function to get end of line position.
  (let ((eol (progn (end-of-line 1) (point)))) ; IN
    ; Let's go to the beginning of the line.
    (beginning-of-line 1)
    ; (message (thing-at-point 'line)) ; DEBUG
    (if (re-search-forward focalize-keywords eol t)
        (let ((found_token (match-string 0))) ; IN
          found_token)
      ; ELSE
      "")
    ) ; END LET
  )


(defun last-keyword-of-line ()
  ; Local function to get beginning of line position.
  (let ((bol (progn (beginning-of-line 1) (point)))) ; IN
    ; Go to the end of the line.
    (end-of-line 1)
    ; (message (thing-at-point 'line)) ; DEBUG
    (if (re-search-backward focalize-keywords bol t)
        (let ((found_token (match-string 0))) ; IN
          found_token)
      ; ELSE
      "")
    ) ; END LET
  )


;; Compute the delta a line to indent must have toward LEFT depending on the
;; on the token it is started by.
(defun parse-line-to-indent-and-compute-indent ()
  (let ((start_kw (first-keyword-of-line))) ; IN
    (cond
     ((member start_kw '("else" "end"))
      (- 0 focalize-indent-quantum))
     (t    ; DEFAULT
      0)
     ) ; END COND
    ) ; END LET
  )


;; Compute the delta a line to indent must have toward RIGHT depending
;; on the token ending the current previous line. By hypothesis, we are on the
;; first non-empty line before the line we want to indent.
(defun parse-line-before-and-compute-indent ()
  (let ((end_kw (last-keyword-of-line))) ; IN
    (cond
     ((member
       end_kw '("else" "then" "begin" "species" "collection"))
      focalize-indent-quantum)
     (t    ; DEFAULT
      0)
     ) ; END COND
    ) ; END LET
  )


;; Search and go to the first non empty line above the current position.
(defun find-and-go-previous-non-empty-line ()
  "Looks for the first previous non-empty line containing something else than only indent characters."
  (setq beg (line-beginning-position))
  (let
      ((found (re-search-backward "[^ \t\n]" nil t 1))) ; IN
    (if found
        ; THEN
        (progn
          (setq the_end (line-beginning-position))
          (let ((displace (count-lines beg the_end))) ; IN
            (message "Jump: %d" displace)
            displace) ; END LET
          ) ; END PROGN
      ; ELSE
      0)
    ) ; END LET
  )


;; Count the number of indentation characters of the current line. ATTENTION:
;; this forces going to the beginning of the current line.
(defun count-current-line-indent ()
  "Count the number of columns between the beginning of the line and the first character that is different of indentation separators."
   ; Let's go to the beginning of this line.
  (beginning-of-line)
  ; Replace tabs by spaces.
  (untabify (point-at-bol) (point-at-eol))
  ; Let's remind its position.
  (let
      ((beg (current-column))) ; IN
    ; Let's go to the first character different of indentation.
    (back-to-indentation)
    ; Compute the horizontal displacement this represents.
    (- (current-column) beg)
    ) ; END LET
  )


;; Indent the current line. By hypothesis, we are at the beginning of the
;; current line.
(defun focalize-indent-current ()
  (if (> (line-beginning-position) 1) ; THEN
      ; Ok, we are not on the first line of the buffer. So let's work.
      (save-excursion
        (let*  ; Let's go to the previous non-empty line.
            ((nb_jmps (find-and-go-previous-non-empty-line))
             ; Get the indent amount of this previous line.
             (prev_indent_amount (count-current-line-indent))
             ; Get the delta the line to indent must have toward RIGHT
             ; depending on the token ending this previous line.
             (from_prev_line (parse-line-before-and-compute-indent))) ; IN
          ; We return to the line we want to indent.
          (forward-line nb_jmps)
          (let
              ; Get the delta the line to indent must have toward LEFT
              ; depending on the token it is started by.
              ((from_current_line (parse-line-to-indent-and-compute-indent)))
              ; IN
              ; And finally, really indent with the found amount.
            (message
             "prev_indent_amount: %d from_prev_line:%d from_current_line:%d"
             prev_indent_amount from_prev_line from_current_line)
            (indent-line-to
             (+ from_current_line (+ from_prev_line prev_indent_amount)))
            ) ; END LET
          ) ; END LET*
        )
    ; ELSE
    ()   ; Do nothing.
    )
  )


;; Indent one general line then restore cursor position.
(defun focalize-indent-line ()
  "FoCaLize mode version of the indent-line-function."
  (interactive "*")
  (let (
        (starting-point ; =
         (point-marker)) ; Let's define 'starting-point' as an object "MARKER"
                         ; and initialise it with the current position.
        ) ; IN
    (beginning-of-line) ; Go to the beginning of the line.
    (focalize-indent-current)    ; Really indent.
    ; Go back to the initial position if we are now placed before.
    (if (< (point) starting-point)   ; 'point' returns the current position
                                     ; as an integer.
        ; THEN
        (goto-char starting-point))
    ; Erase the MARKER 'starting-point'.
    (set-marker starting-point nil)
    )
  ) ; End of 'focalize-indent-line'.


;; Specify the class of some punctuation characters so that various Emacs
;; functions will know what it a "word".
(defvar focalize-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?\' "\"" table)
    (setq focalize-mode-syntax-table table)))


;; Set various variables.
(defun focalize-mode-variables ()
  ; Hook the syntax table.
  (set-syntax-table focalize-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "%%\\|$\\|" page-delimiter)) ;'%%..'
  ; The regex pattern to use for creating a buffer index.
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression "^[a-z][a-zA-Z0-9_]+")
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'focalize-indent-line)
  ; Standard uni-line comment.
  (make-local-variable 'comment-start)
  (setq comment-start "\\(--\\|\(\*\\)")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "\\(--\\|\(\*\\) *")
  (make-local-variable 'comment-column)
  (setq comment-column 48)
  )


(defconst focalize-keywords
  (concat "\\<"
    (regexp-opt
     '("alias" "all" "and" "as" "assume" "assumed" "begin" "by"
       "caml" "collection" "conclude" "coq" "coq_require" "definition"
       "else" "end" "ex" "external" "false" "function" "hypothesis"
       "if" "in" "inherit" "internal" "implement" "is" "let"
       "lexicographic" "local" "logical" "match" "measure" "not"
       "notation" "of" "on" "open" "or" "order" "proof" "prop"
       "property" "prove" "qed" "rec" "representation" "Self"
       "signature" "species" "step" "structural" "termination" "then"
       "theorem" "true" "type" "use" "with") t)
    "\\>")
  )


;; Stuff to highlight.
(defvar focalize-font-lock-keywords
  (eval-when-compile
    (list
     ;; Various operators
     (list "\\(\\+\\|\\(-[^!]\\)\\|\\*\\|/\\|<\\|>\\|=\\|#\\)"
           '(1 font-lock-builtin-face t))

     ;; Constants
     (list "\\<\\(true\\|false\\|[0-9]+\\)\\>"
           '(1 font-lock-constant-face t))

     ;; Keywords defined above.
     (list focalize-keywords '(1 font-lock-keyword-face t))

     ;;  Comments
     (list "\\(--.*\n\\)" '(1 font-lock-comment-face t))
     (list "\\(\(\\*.*\\*\)\\)" '(1 font-lock-comment-face t))
     (list "\\(\{\\*.*\\*\}\\)" '(1 font-lock-comment-face t))
     (list "\\(\(\\*\*.*\\*\)\\)" '(1 font-lock-comment-face t))
     ))
  "Stuff to highlight.")


;; Start the main load.
(defun focalize-mode ()
  "FoCaLize source code edition mode."
  (interactive)
  (kill-all-local-variables)
  ; Activate keymap.
  (use-local-map focalize-mode-map)
  ; Assign the various variables.
  (focalize-mode-variables)
  ; Activate font highlight.
  (set
   (make-local-variable 'font-lock-defaults)
   '(focalize-font-lock-keywords
     nil  ; We do not only want to highlight keywords: strings also.
     t    ; Do not take into consideration the class of the tokens.
     ((?\_ . "w") (?# . "."))
     beginning-of-line   ; Function used to jump outside the current syntactic
                         ; block.
     (font-lock-syntactic-keywords)))
  ; Set mode.
  (setq major-mode 'focalize-mode)
  (setq mode-name "FoCaLize")
  ; Force string search to be insentitive.
  (setq case-fold-search nil)
  ; Don't know why ^_^ ... but must always be called...
  (run-mode-hooks 'focalize-mode-hook)
  )


(provide 'focalize)
