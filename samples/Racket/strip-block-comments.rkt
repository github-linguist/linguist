#lang at-exp racket

;; default delimiters (strings -- not regexps)
(define comment-start-str "/*")
(define comment-end-str "*/")

(define (strip-comments text [rx1 comment-start-str] [rx2 comment-end-str])
  (regexp-replace* (~a (regexp-quote rx1) ".*?" (regexp-quote rx2))
                   text ""))

((compose1 displayln strip-comments)
 @~a{/**
      * Some comments
      * longer comments here that we can parse.
      *
      * Rahoo
      */
      function subroutine() {
       a = /* inline comment */ b + c ;
      }
      /*/ <-- tricky comments */

      /**
       * Another comment.
       */
       function something() {
       }
    })
