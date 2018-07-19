#lang racket
(require racket/dict)

(define (RPN->infix expr)
  (define-values (res _)
    (for/fold ([stack '()] [prec '()]) ([t expr])
      (show t stack prec)
      (cond
        [(dict-has-key? operators t)
         (match-define (list pt at) (dict-ref operators t))
         (match-define (list y x ss ...) stack)
         (match-define (list py px ps ...) prec)
         (define fexpr
           (cond
             [(> pt (max px py)) "(~a) ~a (~a)"]
             [(or (< px pt) (and (= pt px) (eq? at 'r))) "(~a) ~a ~a"]
             [(or (< py pt) (and (= pt py) (eq? at 'l))) "~a ~a (~a)"]
             [else "~a ~a ~a"]))
         (define term (format fexpr x t y))
         (values (cons term ss) (cons pt ps))]
        [else (values (cons t stack) (cons +inf.0 prec))])))
  (car res))

;; the list of operators and their properties
(define operators '((+ 2 l) (- 2 l) (* 3 l) (/ 3 l) (^ 4 r)))

;; printing out the intermediate stages
(define (show t stack prec)
  (printf "~a\t" t)
  (for ([s stack] [p prec])
    (if (eq? +inf.0 p) (printf "[~a] " s) (printf "[~a {~a}] " s p)))
  (newline))
