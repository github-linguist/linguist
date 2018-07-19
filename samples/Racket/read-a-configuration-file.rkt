#lang racket

(require "options.rkt")

(read-options "options-file")
(define-options fullname favouritefruit needspeeling seedsremoved otherfamily)
(printf "fullname       = ~s\n" fullname)
(printf "favouritefruit = ~s\n" favouritefruit)
(printf "needspeeling   = ~s\n" needspeeling)
(printf "seedsremoved   = ~s\n" seedsremoved)
(printf "otherfamily    = ~s\n" otherfamily)
