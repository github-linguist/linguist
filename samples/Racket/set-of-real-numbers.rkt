#lang racket

;; Use a macro to allow infix operators
(require (only-in racket [#%app #%%app]))
(define-for-syntax infixes '())
(define-syntax (definfix stx)
  (syntax-case stx ()
    [(_ (x . xs) body ...) #'(definfix x (λ xs body ...))]
    [(_ x body) (begin (set! infixes (cons #'x infixes)) #'(define x body))]))
(define-syntax (#%app stx)
  (syntax-case stx ()
    [(_ X op Y)
     (and (identifier? #'op) (ormap (λ(o) (free-identifier=? #'op o)) infixes))
     #'(#%%app op X Y)]
    [(_ f x ...) #'(#%%app f x ...)]))


;; Ranges: (X +-+ Y) => [X,Y]; (X --- Y) => (X,Y); and same for `+--' and `--+'
;; Simple implementation as functions

;; Constructors
(definfix ((+-+ X Y) n) (<= X n Y))             ; [X,Y]
(definfix ((--- X Y) n) (< X n Y))              ; (X,Y)
(definfix ((+-- X Y) n) (and (<= X n) (< n Y))) ; [X,Y)
(definfix ((--+ X Y) n) (and (< X n) (<= n Y))) ; (X,Y]
(definfix ((== X) n) (= X n))                   ; [X,X]
;; Set operations
(definfix ((∪ . Rs) n)  (ormap  (λ(p) (p n)) Rs))
(definfix ((∩ . Rs) n)  (andmap (λ(p) (p n)) Rs))
(definfix ((∖ R1 R2) n) (and (R1 n) (not (R2 n)))) ; set-minus, not backslash
(define ((¬ R) n) (not (R n)))
;; Special sets
(define (∅ n) #f)
(define (ℜ n) #t)

(define-syntax-rule (try set)
  (apply printf "~a => ~a ~a ~a\n" (~s #:width 23 'set)
         (let ([pred set]) (for/list ([i 3]) (if (pred i) 'Y 'N)))))
(try ((0 --+ 1) ∪ (0 +-- 2)))
(try ((0 +-- 2) ∩ (1 --+ 2)))
(try ((0 +-- 3) ∖ (0 --- 1)))
(try ((0 +-- 3) ∖ (0 +-+ 1)))
