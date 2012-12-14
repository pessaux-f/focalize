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


(defvar focalize-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap used for FoCaLize mode.")


(add-to-list 'auto-mode-alist '("\\.fcl\\'" . focalize-mode))


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
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
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
             "if" "in" "inherits" "internal" "implements" "is" "let"
             "lexicographic" "local" "logical" "match" "measure" "not"
             "notation" "of" "on" "open" "or" "order" "proof" "prop"
             "property" "prove" "qed" "rec" "representation" "Self"
             "signature" "species" "step" "structural" "termination" "then"
             "theorem" "true" "type" "use" "with") t)
          "\\>")
  )


(defvar focalize-font-lock-keywords
  (eval-when-compile
    (list
     ;; Various operators
     (list "\\(\\+\\|\\(-[^!]\\)\\|\\*\\|/\\|<\\|>\\|=\\|#\\)"
           '(1 font-lock-builtin-face t))

     ;; Constants
     (list "\\<\\(true\\|false\\|[0-9]+\\)\\>"
           '(1 font-lock-constant-face t))

     ;; Keywords
     (list focalize-keywords '(1 font-lock-keyword-face t))

     ;;  Comments
     (list "\\(--.*\n\\)" '(1 font-lock-comment-face t))
     (list "\\(\(\\*.*\\*\)\\)" '(1 font-lock-comment-face t))
     (list "\\(\{\\*.*\\*\}\\)" '(1 font-lock-comment-face t))
     (list "\\(\(\\*\*.*\\*\)\\)" '(1 font-lock-comment-face t))
     ))
  "Stuff to highlight.")



;; Start the autoload.
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
