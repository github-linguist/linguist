;; Variable bound to a number:
(define f 10)
f
===> 10
;; Mutation (altering the bound value)
(set! f (+ f f 6))
f
===> 26
;; Assigning a procedure to the same variable:
(set! f (lambda (n) (+ n 12)))
(f 6)
===> 18
;; Assigning the result of an expression to the same variable:
(set! f (f 1))
f
===> 13
;; functional programming:
(apply + '(1 2 3 4 5 6))
===> 21
(set! f (lambda (n) (+ n 100)))
(map f '(1 2 3))
===> (101 102 103)

