#lang racket

(define (common-directory path . paths)
  (string-join
   (let loop ([path  (string-split path "/" #:trim? #f)]
              [paths (map (λ(p) (string-split p "/" #:trim? #f)) paths)])
     (if (and (pair? path)
              (andmap (λ(p) (and (pair? p) (equal? (car p) (car path))))
                      paths))
       (cons (car path) (loop (cdr path) (map cdr paths)))
       '()))
   "/"))

(common-directory
 "/home/user1/tmp/coverage/test"
 "/home/user1/tmp/covert/operator"
 "/home/user1/tmp/coven/members")
;; --> "/home/user1/tmp"
