#lang racket
(regexp-match* #rx"==|!=|=" "a!===b=!=c" #:gap-select? #t #:match-select values)
;; => '("a" ("!=") "" ("==") "b" ("=") "" ("!=") "c")
