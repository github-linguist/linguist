#lang racket

(require xml)

(define xexpr
  `(html
    (head)
    (body
     (table
      (tr (td) (td "X") (td "Y") (td "Z"))
      ,@(for/list ([i (in-range 1 4)])
          `(tr (td ,(~a i))
               (td ,(~a (random 10000)))
               (td ,(~a (random 10000)))
               (td ,(~a (random 10000)))))))))

(display-xml/content (xexpr->xml xexpr))
