;; *********************************************************************
;;
;;                        FoCaLiZe compiler
;;
;;            François Pessaux
;;
;;                 LIP6 - INRIA Rocquencourt - ENSTA ParisTech
;;
;;  Copyright 2007 - ... LIP6 and INRIA
;;            2012 - ... ENSTA ParisTech
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
    (if (re-search-forward focalize-keywords-and-punct eol t)
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
    (if (re-search-backward focalize-keywords-and-punct bol t)
        (let ((found_token (match-string 0))) ; IN
          ; (message "Found last: %s" found_token)
          found_token)
      ; ELSE
      "")
    ) ; END LET
  )


;; Check if the last keyword or punctuation of the line is a ;; This must be
;; used only when the last keyword or punctuation of the line was detected as
;; being a ;. This is to solve ambiguity between ; and ;; when matching in
;; backward.
(defun check-if-last-keyword-of-line-is-semi-semi ()
  (let ((bol (progn (beginning-of-line 1) (point)))) ; IN
    ; Go to the end of the line.
    (end-of-line 1)
    (re-search-backward ";;" bol t)
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
       end_kw '("let" "else" "then" "begin" "species" "collection" "="))
      focalize-indent-quantum)
     ((member end_kw '(";"))
      ; Check if we found 1 or 2 semi.
      (if (check-if-last-keyword-of-line-is-semi-semi)
        ; THEN
        ; End of a species or a collection: go back full left.
        (- 0 (count-current-line-indent))
        ; ELSE
        ; In case of semi, we privilegiate the case corresponding to an end of
        ; method: Hence, we go back to the first indentation level (i.e. the
        ; one of the hosting species.
        (+ (- 0 (count-current-line-indent)) focalize-indent-quantum)))
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
            ; (message "Jump: %d" displace)
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
;; current line. Returns the new position after indentation. If no indentation
;; was done then it is the same as before. Otherwise it is the point where
;; we arrived due to the indentation.
;; We need to return this position because save-excursion restores the state
;; of the buffer and positions at its end of execution.
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
            ; (message
            ;  "prev_indent_amount: %d from_prev_line:%d from_current_line:%d"
            ;  prev_indent_amount from_prev_line from_current_line)
            (indent-line-to
             (+ from_current_line (+ from_prev_line prev_indent_amount)))
            (point)
            ) ; END LET
          ) ; END LET*
        )
    ; ELSE
    (point)
    )    ; End of if.
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
    (let (
          (new-point
           (focalize-indent-current))    ; Really indent.
          ) ; IN
      ; Go back to the initial position if we are now placed before else go to
      ; the new position (indentation point).
      (if (< new-point starting-point)
          ; THEN
          (goto-char starting-point)
          ; ELSE
          (goto-char new-point))
      ) ; End of let.
    ; Erase the MARKER 'starting-point'.
    (set-marker starting-point nil)
    )
  ) ; End of 'focalize-indent-line'.


;; Specify the class of some punctuation characters so that various Emacs
;; functions will know what it a "word".
(defvar focalize-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Underscore is a valid part of a word. Removed to allow forward-word
    ; to stop on _.
    ; (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?\\ "\\" table)
    ;; Open paren is the start of a two-character comment sequence ('1') and
    ;; is an opening paired-character to match with the rparen.
    (modify-syntax-entry ?( "()1" table)
    ;; Close paren is the end of a two-character comment-start sequence ('4')
    ;; and is a closing paired-character to match with the lparen.
    (modify-syntax-entry ?) ")(4" table)
    ;; Star is the second character of a two-character comment-start sequence
    ;; ('2') or the start of a two-character comment-end sequence ('3').
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    ;; Newline ends a "b-style" comment.
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


;; Regexp matching keywords. This is used for highlightign stuff. This list
;; serves as the basis of stuf used (below) for indentation.
(defconst focalize-keywords
  (concat "\\<"
    (regexp-opt
     '("alias" "all" "and" "as" "assume" "assumed" "begin" "by"
       "caml" "collection" "conclude" "coq" "coq_require" "definition"
       "else" "end" "ex" "external" "false" "final" "function" "hypothesis"
       "if" "in" "inherit" "internal" "implement" "is" "let"
       "lexicographic" "local" "logical" "match" "measure" "not"
       "notation" "of" "on" "open" "or" "order" "proof" "prop"
       "property" "prove" "qed" "rec" "representation" "Self"
       "signature" "species" "step" "structural" "termination" "then"
       "theorem" "true" "type" "use" "with") t)
    "\\>"
    )
  )


;; Regexp matching keywords and (some) punctuation. This regexp is used
;; for indentation since this latter do not only rely on keywords.
(defconst focalize-keywords-and-punct
  (concat focalize-keywords "\\|" (regexp-opt '(";" "=") t))
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
     (list "\\(\(\\*\\*.*\\*\)\\)" '(1 font-lock-comment-face t))
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
