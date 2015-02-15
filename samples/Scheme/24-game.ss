#lang scheme
(require srfi/27 srfi/1) ;; random-integer, every

(define (play)
  (let* ([numbers (build-list 4 (lambda (n)
                                  (add1 (random-integer 9))))]
         [valid?  (curryr valid? numbers)])
    (printf startup-message numbers)
    (let loop ([exp (read)])
      (with-handlers ([exn:fail? (lambda (err)
                                   (printf error-message exp (exn-message err))
                                   (loop (read)))])
       (cond [(eq? exp '!) (play)]

             [(or (eq? exp 'q)
                  (eof-object? exp)) (printf quit-message)]

             [(not (valid? exp))
              (printf bad-exp-message exp)
              (loop (read))]

             [(not (= (eval exp) 24))
              (printf bad-result-message exp (eval exp))
              (loop (read))]

             [else (printf winning-message)])))))

(define (valid? exp numbers)
  ;; must contain each number exactly once and only valid symbols
  (define (valid-symbol? sym)
    ;; only +, -, *, and / are valid
    (case sym
      [(+ - * /) #t]
      [else #f]))

  (let* ([ls (flatten exp)]
         [numbers* (filter number? ls)]
         [symbols  (remove number? ls)])
    (and (equal? (sort numbers <)
                 (sort numbers* <))
         (every valid-symbol? symbols))))

(define startup-message "
Write a lisp expression that evaluates to 24
using only (, ), +, -, *, /
and these four numbers: ~a

or '!' to get a new set of numbers
or 'q' to quit")

(define error-message "
Your expression ~a raised an exception:

  \"~a\"

Please try again")

(define bad-exp-message "Sorry, ~a is a bad expression.")
(define bad-result-message "Sorry, ~a evaluates to ~a, not 24.")
(define quit-message "Thanks for playing...")
(define winning-message "You win!")

(provide play)
