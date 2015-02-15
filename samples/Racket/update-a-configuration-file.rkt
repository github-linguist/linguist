#lang racket

(require "options.rkt")


(read-options "options-file")
(define-options needspeeling seedsremoved numberofbananas numberofstrawberries)

;; Disable the needspeeling option (using a semicolon prefix)
(set! needspeeling #f)
;; Enable the seedsremoved option by removing the semicolon and any
;; leading whitespace
(set! seedsremoved ENABLE)
;; Change the numberofbananas parameter to 1024
(set! numberofbananas 1024)
;; Enable (or create if it does not exist in the file) a parameter for
;; numberofstrawberries with a value of 62000
(set! numberofstrawberries 62000)
