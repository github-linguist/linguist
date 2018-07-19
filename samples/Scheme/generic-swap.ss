; swap elements of a vector
; vector-swap! is not part of r5rs, so we define it
(define (vector-swap! v i j)
(let ((a (vector-ref v i)) (b (vector-ref v j)))
(vector-set! v i b)
(vector-set! v j a)))

(let ((vec (vector 1 2 3 4 5)))
  (vector-swap! vec 0 4)
  vec)
; #(5 2 3 4 1)


; we can swap also in lists
(define (list-swap! v i j)
(let* ((x (list-tail v i))
       (y (list-tail v j))
       (a (car x))
       (b (car y)))
(set-car! x b)
(set-car! y a)))

(let ((lis (list 1 2 3 4 5)))
   (list-swap! lis 0 4)
   lis)
; (5 2 3 4 1)


; using macros (will work on variables, not on vectors or lists)
(define-syntax swap!
(syntax-rules ()
((_ a b)
   (let ((tmp a))
   (set! a b)
   (set! b tmp)))))

; try it
(let ((a 1) (b 2)) (swap! a b) (list a b))
; (2 1)
