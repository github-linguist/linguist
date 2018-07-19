#lang racket

;; A quick `amb' implementation
(define failures null)
(define (fail)
  (if (pair? failures) ((first failures)) (error "no more choices!")))
(define (amb/thunks choices)
  (let/cc k (set! failures (cons k failures)))
  (if (pair? choices)
    (let ([choice (first choices)]) (set! choices (rest choices)) (choice))
    (begin (set! failures (rest failures)) (fail))))
(define-syntax-rule (amb E ...) (amb/thunks (list (lambda () E) ...)))
(define (assert condition) (unless condition (fail)))

;; just to make things more fun
(define (⇔ x y) (assert (eq? x y)))
(require (only-in racket [and ∧] [or ∨] [implies ⇒] [xor ⊻] [not ¬]))
(define (count xs)
  (let loop ([n 0] [xs xs])
    (if (null? xs) n (loop (if (car xs) (add1 n) n) (cdr xs)))))
;; even more fun, make []s infix
(require (only-in racket [#%app r:app]))
(define-syntax (#%app stx)
  (if (not (eq? #\[ (syntax-property stx 'paren-shape)))
    (syntax-case stx () [(_ x ...) #'(r:app x ...)])
    (syntax-case stx ()
      ;; extreme hack on next two cases, so it works for macros too.
      [(_ x op y) (syntax-property #'(op x y) 'paren-shape #f)]
      [(_ x op y op1 z) (free-identifier=? #'op #'op1)
       (syntax-property #'(op x y z) 'paren-shape #f)])))
;; might as well do more
(define-syntax-rule (define-booleans all x ...)
  (begin (define x (amb #t #f)) ...
         (define all (list x ...))))

(define (puzzle)
  (define-booleans all q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12)
  ;; 1.  This is a numbered list of twelve statements.
  [q1 ⇔ [12 = (length all)]]
  ;; 2.  Exactly 3 of the last 6 statements are true.
  [q2 ⇔ [3 = (count (take-right all 6))]]
  ;; 3.  Exactly 2 of the even-numbered statements are true.
  [q3 ⇔ [2 = (count (list q2 q4 q6 q8 q10 q12))]]
  ;; 4.  If statement 5 is true, then statements 6 and 7 are both true.
  [q4 ⇔ [q5 ⇒ [q6 ∧ q7]]]
  ;; 5.  The 3 preceding statements are all false.
  [q5 ⇔ (¬ [q2 ∨ q3 ∨ q4])]
  ;; 6.  Exactly 4 of the odd-numbered statements are true.
  [q6 ⇔ [4 = (count (list q1 q3 q5 q7 q9 q11))]]
  ;; 7.  Either statement 2 or 3 is true, but not both.
  [q7 ⇔ [q2 ⊻ q3]]
  ;; 8.  If statement 7 is true, then 5 and 6 are both true.
  [q8 ⇔ [q7 ⇒ (and q5 q6)]]
  ;; 9.  Exactly 3 of the first 6 statements are true.
  [q9 ⇔ [3 = (count (take all 3))]]
  ;; 10. The next two statements are both true.
  [q10 ⇔ [q11 ∧ q12]]
  ;; 11. Exactly 1 of statements 7, 8 and 9 are true.
  [q11 ⇔ [1 = (count (list q7 q8 q9))]]
  ;; 12. Exactly 4 of the preceding statements are true.
  [q12 ⇔ [4 = (count (drop-right all 1))]]
  ;; done
  (for/list ([i (in-naturals 1)] [q all] #:when q) i))

(puzzle)
;; -> '(1 3 4 6 7 11)
