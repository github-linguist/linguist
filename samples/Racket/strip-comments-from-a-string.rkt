#lang at-exp racket

(define comment-start-rx "[;#]")

(define text
  @~a{apples, pears # and bananas
      apples, pears ; and bananas
      })

(define (strip-comments text [rx comment-start-rx])
  (string-join
   (for/list ([line (string-split text "\n")])
     (string-trim line (pregexp (~a "\\s*" rx ".*")) #:left? #f))
   "\n"))

;; Alternatively, do it in a single regexp operation
(define (strip-comments2 text [rx comment-start-rx])
  (regexp-replace* (pregexp (~a "(?m:\\s*" rx ".*)")) text ""))

(strip-comments2 text) ; -> "apples, pears\napples, pears"
