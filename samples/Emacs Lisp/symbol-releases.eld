;; Emacs versions when certain symbols and variables were first introduced,
;; for use in `describe-function'.
;;
;; This file is used to explicitly override the heuristic scraping NEWS*
;; files, when that would result in misleading information.
;;
;; It should contain a single list of (VERSION TYPE SYMBOL), where
;; VERSION is the Emacs version when SYMBOL was introduced as a TYPE,
;; TYPE being `fun' or `var'.

(
 ("28.1" fun always)
 ("26.1" fun when-let*)
 ("26.1" fun and-let*)
 ("26.1" fun if-let*)
 ("24.4" fun set-transient-map)
 ("22.1" fun clear-string)
 ("22.1" fun version=)
 ("22.1" fun version<)
 ("22.1" fun version<=)
 ("22.1" fun read-number)

;; Since much of early Emacs source history is lost, these versions are
;; conservative estimates: the actual version of first appearance may very
;; well be much earlier.
;; 13.8 may be the earliest surviving version with source code, although
;; damaged.  See
;; https://github.com/larsbrinkhoff/emacs-history/tree/sources/decuslib.com/decus/vax85b/gnuemax

 ("19.7" fun defsubst)
 ("19.34" fun make-directory)
 ("18.59" fun mark)
 ("13.8" fun nthcdr)
 ("13.8" fun nreverse)
 ("13.8" fun let*)
 ("13.8" fun rassq)
 ("13.8" fun >=)
 ("13.8" fun transpose-sexps)
 ("13.8" fun buffer-modified-p)
 ("13.8" fun current-column)
 ("13.8" fun downcase)
 ("13.8" fun previous-line)
 ("13.8" fun catch)
 ("13.8" fun throw)
 ("13.8" fun count-lines)
 )
