;; ess-julia.el --- ESS julia mode and inferior interaction
;;
;; Copyright (C) 2012 Vitalie Spinu.
;;
;; Filename: ess-julia.el
;; Author: Vitalie Spinu (based on julia-mode.el from julia-lang project)
;; Maintainer: Vitalie Spinu
;; Created: 02-04-2012 (ESS 12.03)
;; Keywords: ESS, julia
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is *NOT* part of GNU Emacs.
;; This file is part of ESS
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;  customise inferior-julia-program-name to point to your julia-release-basic
;;  and start the inferior with M-x julia.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(require 'compile); for compilation-* below

;;; Code:

(defvar julia-mode-hook nil)

(add-to-list 'auto-mode-alist '("\\.jl\\'" . julia-mode))

(defvar julia-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "_" table)   ; underscores in words
    (modify-syntax-entry ?@ "_" table)
    (modify-syntax-entry ?. "_" table)
    (modify-syntax-entry ?# "<" table)   ; #  single-line comment start
    (modify-syntax-entry ?\n ">" table)  ; \n single-line comment end
    (modify-syntax-entry ?\{ "(} " table)
    (modify-syntax-entry ?\} "){ " table)
    (modify-syntax-entry ?\[ "(] " table)
    (modify-syntax-entry ?\] ")[ " table)
    (modify-syntax-entry ?\( "() " table)
    (modify-syntax-entry ?\) ")( " table)
    ;(modify-syntax-entry ?\\ "." table)  ; \ is an operator outside quotes
    (modify-syntax-entry ?'  "." table)  ; character quote or transpose
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?` "\"" table)
    ;; (modify-syntax-entry ?\" "." table)
    (modify-syntax-entry ?? "." table)
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?% "." table)
    table)
  "Syntax table for julia-mode")

;; syntax table that holds within strings
(defvar julia-mode-string-syntax-table
  (let ((table (make-syntax-table)))
    table)
  "Syntax table for julia-mode")

;; disable " inside char quote
(defvar julia-mode-char-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "." table)
    table)
  "Syntax table for julia-mode")

;; not used
;; (defconst julia-string-regex
;;   "\"[^\"]*?\\(\\(\\\\\\\\\\)*\\\\\"[^\"]*?\\)*\"")

(defconst julia-char-regex
  "\\(\\s(\\|\\s-\\|-\\|[,%=<>\\+*/?&|$!\\^~\\\\;:]\\|^\\)\\('\\(\\([^']*?[^\\\\]\\)\\|\\(\\\\\\\\\\)\\)'\\)")

(defconst julia-unquote-regex
  "\\(\\s(\\|\\s-\\|-\\|[,%=<>\\+*/?&|!\\^~\\\\;:]\\|^\\)\\($[a-zA-Z0-9_]+\\)")

(defconst julia-forloop-in-regex
  "for +[^ 	]+ +.*\\(in\\)\\(\\s-\\|$\\)+")

(defconst ess-subset-regexp
      "\\[[0-9:, ]*\\]" )

(defconst julia-font-lock-defaults
  (list '("\\<\\(\\|Uint\\(8\\|16\\|32\\|64\\)\\|Int\\(8\\|16\\|32\\|64\\)\\|Integer\\|Float\\|Float32\\|Float64\\|Complex128\\|Complex64\\|ComplexNum\\|Bool\\|Char\\|Number\\|Scalar\\|Real\\|Int\\|Uint\\|Array\\|DArray\\|AbstractArray\\|AbstractVector\\|AbstractMatrix\\|SubArray\\|StridedArray\\|StridedVector\\|StridedMatrix\\|VecOrMat\\|StridedVecOrMat\\|Range\\|Range1\\|SparseMatrixCSC\\|Tuple\\|NTuple\\|Buffer\\|Size\\|Index\\|Symbol\\|Function\\|Vector\\|Matrix\\|Union\\|Type\\|Any\\|Complex\\|None\\|String\\|Ptr\\|Void\\|Exception\\|PtrInt\\|Long\\|Ulong\\)\\>" .
      font-lock-type-face)
    (cons
     (concat "\\<\\("
         (mapconcat
          'identity
          '("if" "else" "elseif" "while" "for" "begin" "end" "quote"
            "try" "catch" "return" "local" "abstract" "function" "macro" "ccall"
	    "typealias" "break" "continue" "type" "global" "@\\w+"
	    "module" "import" "export" "const" "let" "bitstype" "using")
          "\\|") "\\)\\>")
     'font-lock-keyword-face)
    '("\\<\\(true\\|false\\|C_NULL\\|Inf\\|NaN\\|Inf32\\|NaN32\\)\\>" . font-lock-constant-face)
    (list julia-unquote-regex 2 'font-lock-constant-face)
    (list julia-char-regex 2 'font-lock-string-face)
    (list julia-forloop-in-regex 1 'font-lock-keyword-face)
    ;; (cons ess-subset-regexp 'font-lock-constant-face)
    (cons "\\(\\sw+\\) ?(" '(1 font-lock-function-name-face keep))
    ;(list julia-string-regex 0 'font-lock-string-face)
))

(defconst julia-block-start-keywords
  (list "if" "while" "for" "begin" "try" "function" "type" "let" "macro"
	"quote"))

(defconst julia-block-other-keywords
  (list "else" "elseif"))

(defconst julia-block-end-keywords
  (list "end" "else" "elseif" "catch"))

(defun ess-inside-brackets-p (&optional pos)
  (save-excursion
    (let* ((pos (or pos (point)))
	   (beg (re-search-backward "\\[" (max (point-min) (- pos 1000)) t))
	   (end (re-search-forward "\\]" (min (point-max) (+ pos 1000)) t)))
    (and beg end (> pos beg) (> end pos)))))

(defun julia-at-keyword (kw-list)
  ; not a keyword if used as a field name, X.word, or quoted, :word
  (and (or (= (point) 1)
	   (and (not (equal (char-before (point)) ?.))
		(not (equal (char-before (point)) ?:))))
       (not (ess-inside-string-or-comment-p (point)))
       (not (ess-inside-brackets-p (point)))
       (member (current-word) kw-list)))

; get the position of the last open block
(defun julia-last-open-block-pos (min)
  (let ((count 0))
    (while (not (or (> count 0) (<= (point) min)))
      (backward-word 1)
      (setq count
	    (cond ((julia-at-keyword julia-block-start-keywords)
		   (+ count 1))
		  ((and (equal (current-word) "end")
			(not (ess-inside-comment-p)) (not (ess-inside-brackets-p)))
		   (- count 1))
		  (t count))))
    (if (> count 0)
	(point)
      nil)))

; get indent for last open block
(defun julia-last-open-block (min)
  (let ((pos (julia-last-open-block-pos min)))
    (and pos
	 (progn
	   (goto-char pos)
	   (+ julia-basic-offset (current-indentation))))))

; return indent implied by a special form opening on the previous line, if any
(defun julia-form-indent ()
  (forward-line -1)
  (end-of-line)
  (backward-sexp)
  (if (julia-at-keyword julia-block-other-keywords)
      (+ julia-basic-offset (current-indentation))
    (if (char-equal (char-after (point)) ?\()
        (progn
          (backward-word 1)
          (let ((cur (current-indentation)))
            (if (julia-at-keyword julia-block-start-keywords)
                (+ julia-basic-offset cur)
              nil)))
      nil)))

(defun julia-paren-indent ()
  (let* ((p (parse-partial-sexp (save-excursion
				  ;; only indent by paren if the last open
				  ;; paren is closer than the last open
				  ;; block
				  (or (julia-last-open-block-pos (point-min))
				      (point-min)))
				(progn (beginning-of-line)
				       (point))))
         (pos (cadr p)))
    (if (or (= 0 (car p)) (null pos))
        nil
      (progn (goto-char pos) (+ 1 (current-column))))))
					;  (forward-line -1)
					;  (end-of-line)
					;  (let ((pos (condition-case nil
					;                (scan-lists (point) -1 1)
					;              (error nil))))
					;   (if pos
					;       (progn (goto-char pos) (+ 1 (current-column)))
					;     nil)))

(defun julia-indent-line ()
  "Indent current line of julia code"
  (interactive)
					;  (save-excursion
    (end-of-line)
    (indent-line-to
     (or (and (ess-inside-string-p (point-at-bol)) 0)
	 (save-excursion (ignore-errors (julia-form-indent)))
         (save-excursion (ignore-errors (julia-paren-indent)))
         ;; previous line ends in =
	 (save-excursion
           (beginning-of-line)
           (skip-chars-backward " \t\n")
           (when (eql (char-before) ?=)
             (+ julia-basic-offset (current-indentation))))
         (save-excursion
           (let ((endtok (progn
                           (beginning-of-line)
                           (forward-to-indentation 0)
                           (julia-at-keyword julia-block-end-keywords))))
             (ignore-errors (+ (julia-last-open-block (point-min))
                               (if endtok (- julia-basic-offset) 0)))))
	 ;; take same indentation as previous line
	 (save-excursion (forward-line -1)
			 (current-indentation))
         0))
    (when (julia-at-keyword julia-block-end-keywords)
      (forward-word 1)))

(defvar julia-editing-alist
  '((paragraph-start		  . (concat "\\s-*$\\|" page-delimiter))
    (paragraph-separate		  . (concat "\\s-*$\\|" page-delimiter))
    (paragraph-ignore-fill-prefix . t)
    (require-final-newline	  . t)
    (comment-start		  . "# ")
    (comment-add                  . 1)
    (comment-start-skip		  . "#+\\s-*")
    (comment-column		  . 40)
    ;;(comment-indent-function	. 'S-comment-indent)
    ;;(ess-comment-indent	    . 'S-comment-indent)
    ;; (ess-indent-line			    . 'S-indent-line)
    ;;(ess-calculate-indent	      . 'ess-calculate-indent)
    (ess-indent-line-function	  . 'julia-indent-line)
    (indent-line-function	  . 'julia-indent-line)
    (parse-sexp-ignore-comments	  . t)
    (ess-style		  	  . ess-default-style) ;; ignored
    (ess-local-process-name	  . nil)
    ;;(ess-keep-dump-files	    . 'ask)
    (ess-mode-syntax-table	  . julia-syntax-table)
    ;; For Changelog add, require ' ' before <- : "attr<-" is a function name :
    ;; (add-log-current-defun-header-regexp . "^\\(.+\\)\\s-+=[ \t\n]*function")
    (add-log-current-defun-header-regexp . "^.*function[ \t]*\\([^ \t(]*\\)[ \t]*(")
    (font-lock-defaults		  . '(julia-font-lock-defaults
                                      nil nil ((?\_ . "w"))))
    )
  "General options for julia source files.")

(autoload 'inferior-ess "ess-inf" "Run an ESS process.")
(autoload 'ess-mode     "ess-mode" "Edit an ESS process.")

(defun julia-send-string-function (process string visibly)
  (let ((file (concat temporary-file-directory "julia_eval_region.jl")))
    (with-temp-file file
      (insert string))
    (process-send-string process (format ess-load-command file))))

(defun julia-get-help-topics (&optional proc)
  (ess-get-words-from-vector "ESS.all_help_topics()\n"))
    ;; (ess-command com)))

(defvar julia-help-command "help(\"%s\")\n")

(defvar ess-julia-error-regexp-alist '(julia-in julia-at)
  "List of symbols which are looked up in `compilation-error-regexp-alist-alist'.")

(add-to-list 'compilation-error-regexp-alist-alist
             '(julia-in  "^\\s-*in [^ \t\n]* \\(at \\(.*\\):\\([0-9]+\\)\\)" 2 3 nil 2 1))
(add-to-list 'compilation-error-regexp-alist-alist
             '(julia-at "^\\S-+\\s-+\\(at \\(.*\\):\\([0-9]+\\)\\)"  2 3 nil 2 1))

(defvar julia-customize-alist
  '((comint-use-prompt-regexp		. t)
    (ess-eldoc-function           . 'ess-julia-eldoc-function)
    (inferior-ess-primary-prompt	. "a> ") ;; from julia>
    (inferior-ess-secondary-prompt	. nil)
    (inferior-ess-prompt		. "\\w*> ")
    (ess-local-customize-alist		. 'julia-customize-alist)
    (inferior-ess-program		. inferior-julia-program-name)
    (inferior-ess-font-lock-defaults	. julia-font-lock-defaults)
    (ess-get-help-topics-function	. 'julia-get-help-topics)
    (ess-help-web-search-command        . "http://docs.julialang.org/en/latest/search/?q=%s")
    (ess-load-command   		. "include(\"%s\")\n")
    (ess-funargs-command                . "ESS.fun_args(\"%s\")\n")
    (ess-dump-error-re			. "in \\w* at \\(.*\\):[0-9]+")
    (ess-error-regexp			. "\\(^\\s-*at\\s-*\\(?3:.*\\):\\(?2:[0-9]+\\)\\)")
    (ess-error-regexp-alist		. ess-julia-error-regexp-alist)
    (ess-send-string-function		. nil);'julia-send-string-function)
    (ess-imenu-generic-expression       . julia-imenu-generic-expression)
    ;; (inferior-ess-objects-command	. inferior-R-objects-command)
    ;; (inferior-ess-search-list-command	. "search()\n")
    (inferior-ess-help-command		. julia-help-command)
    ;; (inferior-ess-help-command	. "help(\"%s\")\n")
    (ess-language			. "julia")
    (ess-dialect			. "julia")
    (ess-suffix				. "jl")
    (ess-dump-filename-template		. (ess-replace-regexp-in-string
					   "S$" ess-suffix ; in the one from custom:
					   ess-dump-filename-template-proto))
    (ess-mode-syntax-table		. julia-syntax-table)
    (ess-mode-editing-alist	        . julia-editing-alist)
    (ess-change-sp-regexp		. nil );ess-R-change-sp-regexp)
    (ess-help-sec-regex			. ess-help-R-sec-regex)
    (ess-help-sec-keys-alist		. ess-help-R-sec-keys-alist)
    (ess-loop-timeout			. ess-S-loop-timeout);fixme: dialect spec.
    (ess-cmd-delay			. ess-R-cmd-delay)
    (ess-function-pattern		. ess-R-function-pattern)
    (ess-object-name-db-file		. "ess-r-namedb.el" )
    (ess-smart-operators		. ess-R-smart-operators)
    (inferior-ess-help-filetype        . nil)
    (inferior-ess-exit-command		. "exit()\n")
    ;;harmful for shell-mode's C-a: -- but "necessary" for ESS-help?
    (inferior-ess-start-file		. nil) ;; "~/.ess-R"
    (inferior-ess-start-args		. "")
    (inferior-ess-language-start	. nil)
    (ess-STERM		. "iESS")
    (ess-editor	. R-editor)
    (ess-pager		. R-pager)
    )
  "Variables to customize for Julia -- set up later than emacs initialization.")


(defvar ess-julia-versions '("julia")
  "List of partial strings for versions of Julia to access within ESS.
Each string specifies the start of a filename.  If a filename
beginning with one of these strings is found on `exec-path', a M-x
command for that version of Julia is made available.  ")

(defcustom inferior-julia-args ""
  "String of arguments (see 'julia --help') used when starting julia."
;; These arguments are currently not passed to other versions of julia that have
;; been created using the variable `ess-r-versions'."
  :group 'ess-julia
  :type 'string)

;;;###autoload
(defun julia-mode  (&optional proc-name)
  "Major mode for editing julia source.  See `ess-mode' for more help."
  (interactive "P")
  ;; (setq ess-customize-alist julia-customize-alist)
  (ess-mode julia-customize-alist proc-name)
  ;; for emacs < 24
  ;; (add-hook 'comint-dynamic-complete-functions 'ess-complete-object-name nil 'local)
  ;; for emacs >= 24
  ;; (remove-hook 'completion-at-point-functions 'ess-filename-completion 'local) ;; should be first
  ;; (add-hook 'completion-at-point-functions 'ess-object-completion nil 'local)
  ;; (add-hook 'completion-at-point-functions 'ess-filename-completion nil 'local)
  (if (fboundp 'ess-add-toolbar) (ess-add-toolbar))
  (set (make-local-variable 'end-of-defun-function) 'ess-end-of-function)
  ;; (local-set-key  "\t" 'julia-indent-line) ;; temp workaround
  ;; (set (make-local-variable 'indent-line-function) 'julia-indent-line)
  (set (make-local-variable 'julia-basic-offset) 4)
  (setq imenu-generic-expression julia-imenu-generic-expression)
  (imenu-add-to-menubar "Imenu-jl")
  (run-hooks 'julia-mode-hook))


(defvar ess-julia-post-run-hook nil
  "Functions run in process buffer after the initialization of
  julia process.")

;;;###autoload
(defun julia (&optional start-args)
  "Call 'julia',
Optional prefix (C-u) allows to set command line arguments, such as
--load=<file>.  This should be OS agnostic.
If you have certain command line arguments that should always be passed
to julia, put them in the variable `inferior-julia-args'."
  (interactive "P")
  ;; get settings, notably inferior-julia-program-name :
  (if (null inferior-julia-program-name)
      (error "'inferior-julia-program-name' does not point to 'julia-release-basic' executable")
    (setq ess-customize-alist julia-customize-alist)
    (ess-write-to-dribble-buffer   ;; for debugging only
     (format
      "\n(julia): ess-dialect=%s, buf=%s, start-arg=%s\n current-prefix-arg=%s\n"
      ess-dialect (current-buffer) start-args current-prefix-arg))
    (let* ((jl-start-args
	    (concat inferior-julia-args " " ; add space just in case
		    (if start-args
			(read-string
                         (concat "Starting Args"
                                 (if inferior-julia-args
                                     (concat " [other than '" inferior-julia-args "']"))
                                 " ? "))
		      nil))))
      (inferior-ess jl-start-args) ;; -> .. (ess-multi ...) -> .. (inferior-ess-mode) ..
      (ess--tb-start)
      (set (make-local-variable 'julia-basic-offset) 4)
      ;; remove ` from julia's logo
      (goto-char (point-min))
      (while (re-search-forward "`" nil t)
        (replace-match "'"))
      (goto-char (point-max))
      (ess--inject-code-from-file (format "%sess-julia.jl" ess-etc-directory))
      (with-ess-process-buffer nil
        (run-mode-hooks 'ess-julia-post-run-hook))
      )))

;;; ELDOC

(defun ess-julia-eldoc-function ()
  "Return the doc string, or nil.
If an ESS process is not associated with the buffer, do not try
to look up any doc strings."
  (interactive)
  (when (and (ess-process-live-p)
             (not (ess-process-get 'busy)))
    (let ((funname (or (and ess-eldoc-show-on-symbol ;; aggressive completion
                            (symbol-at-point))
                       (car (ess--funname.start)))))
      (when funname
        (let* ((args (copy-sequence (nth 2 (ess-function-arguments funname))))
               (W (- (window-width (minibuffer-window)) (+ 4 (length funname))))
               (doc (concat (propertize funname 'face font-lock-function-name-face) ": ")))
          (when args
            (setq args (sort args (lambda (s1 s2)
                                    (< (length s1) (length s2)))))
            (setq doc (concat doc (pop args)))
            (while (and args (< (length doc) W))
              (setq doc (concat doc "  "
                                (pop args))))
            (when (and args (< (length doc) W))
              (setq doc (concat doc " {--}"))))
          doc)))))


;;; IMENU
(defvar julia-imenu-generic-expression
  ;; don't use syntax classes, screws egrep
  '(("Function (_)" "[ \t]*function[ \t]+\\(_[^ \t\n]*\\)" 1)
    ("Function" "[ \t]*function[ \t]+\\([^_][^\t\n]*\\)" 1)
    ("Const" "[ \t]*const \\([^ \t\n]*\\)" 1)
    ("Type"  "^[ \t]*[a-zA-Z0-9_]*type[a-zA-Z0-9_]* \\([^ \t\n]*\\)" 1)
    ("Require"      " *\\(\\brequire\\)(\\([^ \t\n)]*\\)" 2)
    ("Include"      " *\\(\\binclude\\)(\\([^ \t\n)]*\\)" 2)
    ;; ("Classes" "^.*setClass(\\(.*\\)," 1)
    ;; ("Coercions" "^.*setAs(\\([^,]+,[^,]*\\)," 1) ; show from and to
    ;; ("Generics" "^.*setGeneric(\\([^,]*\\)," 1)
    ;; ("Methods" "^.*set\\(Group\\|Replace\\)?Method(\"\\(.+\\)\"," 2)
    ;; ;;[ ]*\\(signature=\\)?(\\(.*,?\\)*\\)," 1)
    ;; ;;
    ;; ;;("Other" "^\\(.+\\)\\s-*<-[ \t\n]*[^\\(function\\|read\\|.*data\.frame\\)]" 1)
    ;; ("Package" "^.*\\(library\\|require\\)(\\(.*\\)," 2)
    ;; ("Data" "^\\(.+\\)\\s-*<-[ \t\n]*\\(read\\|.*data\.frame\\).*(" 1)))
    ))

(provide 'ess-julia)
