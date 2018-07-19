#lang racket
(displayln
 (if (regexp-match? #px"(?i:utf-?8)"
                    (or (getenv "LC_ALL") (getenv "LC_CTYPE") (getenv "LANG")))
   "\u25b3" "No Unicode detected."))
