; auxiliary functions : range, filter
(define (range a b)
(let loop ((v '()) (i b))
(if (< i a)
    v
    (loop (cons i v)
          (- i 1)))))

(define (filter p u)
(if (equal? u '())
    '()
    (let ((x (car u)) (v (filter p (cdr u))))
         (if (p x)
             (cons x v)
             v))))

(define (kaprekar? n)
(or (= n 1)
    (let ((q (* n n)))
    (let loop ((p 10))
         (cond ((> p q) #f)
               ((let ((a (remainder q p)) (b (quotient q p)))
                     (and (> a 0) (= n (+ a b)))) #t)
               (else (loop (* p 10))))))))

(filter kaprekar? (range 1 10000))
; (1 9 45 55 99 297 703 999 2223 2728 4879 4950 5050 5292 7272 7777 9999)
