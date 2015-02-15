#lang racket (require math)

(define sqrt5 (sqrt 5))
(define phi (* 0.5 (+ 1 sqrt5)))

;; What is the nth fibonnaci number, shifted by 2 so that
;; F(0) = 1, F(1) = 2, ...?
;;
(define (F n)
  (fibonacci (+ n 2)))

;; What is the largest n such that F(n) <= m?
;;
(define (F* m)
  (let ([n (- (inexact->exact (round (/ (log (* m sqrt5)) (log phi)))) 2)])
    (if (<= (F n) m) n (sub1 n))))

(define (zeck->natural z)
  (for/sum ([i (reverse z)]
            [j (in-naturals)])
    (* i (F j))))

(define (natural->zeck n)
  (if (zero? n)
      null
      (for/list ([i (in-range (F* n) -1 -1)])
        (let ([f (F i)])
          (cond [(>= n f) (set! n (- n f))
                          1]
                [else 0])))))

; Extend list to the right to a length of len with repeated padding elements
;
(define (pad lst len [padding 0])
  (append lst (make-list (- len (length lst)) padding)))

; Strip padding elements from the left of the list
;
(define (unpad lst [padding 0])
  (cond [(null? lst) lst]
        [(equal? (first lst) padding) (unpad (rest lst) padding)]
        [else lst]))

;; Run a filter function across a window in a list from left to right
;;
(define (left->right width fn)
  (λ (lst)
    (let F ([a lst])
      (if (< (length a) width)
          a
          (let ([f (fn (take a width))])
            (cons (first f) (F (append (rest f) (drop a width)))))))))

;; Run a function fn across a window in a list from right to left
;;
(define (right->left width fn)
  (λ (lst)
    (let F ([a lst])
      (if (< (length a) width)
          a
          (let ([f (fn (take-right a width))])
            (append (F (append (drop-right a width) (drop-right f 1)))
                    (list (last f))))))))

;; (a0 a1 a2 ... an) -> (a0 a1 a2 ... (fn ... an))
;;
(define (replace-tail width fn)
  (λ (lst)
    (append (drop-right lst width) (fn (take-right lst width)))))

(define (rule-a lst)
  (match lst
    [(list 0 2 0 x) (list 1 0 0 (add1 x))]
    [(list 0 3 0 x) (list 1 1 0 (add1 x))]
    [(list 0 2 1 x) (list 1 1 0 x)]
    [(list 0 1 2 x) (list 1 0 1 x)]
    [else lst]))

(define (rule-a-tail lst)
  (match lst
    [(list x 0 3 0) (list x 1 1 1)]
    [(list x 0 2 0) (list x 1 0 1)]
    [(list 0 1 2 0) (list 1 0 1 0)]
    [(list x y 0 3) (list x y 1 1)]
    [(list x y 0 2) (list x y 1 0)]
    [(list x 0 1 2) (list x 1 0 0)]
    [else lst]))

(define (rule-b lst)
  (match lst
    [(list 0 1 1) (list 1 0 0)]
    [else lst]))

(define (rule-c lst)
  (match lst
    [(list 1 0 0) (list 0 1 1)]
    [(list 1 -1 0) (list 0 0 1)]
    [(list 1 -1 1) (list 0 0 2)]
    [(list 1 0 -1) (list 0 1 0)]
    [(list 2 0 0) (list 1 1 1)]
    [(list 2 -1 0) (list 1 0 1)]
    [(list 2 -1 1) (list 1 0 2)]
    [(list 2 0 -1) (list 1 1 0)]
    [else lst]))

(define (zeck-combine op y z [f identity])
  (let* ([bits (max (add1 (length y)) (add1 (length z)) 4)]
         [f0 (λ (x) (pad (reverse x) bits))]
         [f1 (left->right 4 rule-a)]
         [f2 (replace-tail 4 rule-a-tail)]
         [f3 (right->left 3 rule-b)]
         [f4 (left->right 3 rule-b)])
    ((compose1 unpad f4 f3 f2 f1 f reverse) (map op (f0 y) (f0 z)))))

(define (zeck+ y z)
  (zeck-combine + y z))

(define (zeck- y z)
  (when (zeck< y z) (error (format "~a" `(zeck-: cannot subtract since ,y < ,z))))
  (zeck-combine - y z (left->right 3 rule-c)))

(define (zeck* y z)
  (define (M ry Zn Zn_1 [acc null])
    (if (null? ry)
        acc
        (M (rest ry) (zeck+ Zn Zn_1) Zn
           (if (zero? (first ry)) acc (zeck+ acc Zn)))))
  (cond [(zeck< z y) (zeck* z y)]
        [(null? y) null]               ; 0 * z -> 0
        [else (M (reverse y) z z)]))

(define (zeck-quotient/remainder y z)
  (define (M Zn acc)
    (if (zeck< y Zn)
        (drop-right acc 1)
        (M (zeck+ Zn (first acc)) (cons Zn acc))))
  (define (D x m [acc null])
    (if (null? m)
        (values (reverse acc) x)
        (let* ([v (first m)]
               [smaller (zeck< v x)]
               [bit (if smaller 1 0)]
               [x_ (if smaller (zeck- x v) x)])
          (D x_ (rest m) (cons bit acc)))))
  (D y (M z (list z))))

(define (zeck-quotient y z)
  (let-values ([(quotient _) (zeck-quotient/remainder y z)])
    quotient))

(define (zeck-remainder y z)
  (let-values ([(_ remainder) (zeck-quotient/remainder y z)])
    remainder))

(define (zeck-add1 z)
  (zeck+ z '(1)))

(define (zeck= y z)
  (equal? (unpad y) (unpad z)))

(define (zeck< y z)
  ; Compare equal-length unpadded zecks
  (define (LT a b)
    (if (null? a)
        #f
        (let ([a0 (first a)] [b0 (first b)])
          (if (= a0 b0)
              (LT (rest a) (rest b))
              (= a0 0)))))

  (let* ([a (unpad y)] [len-a (length a)]
         [b (unpad z)] [len-b (length b)])
    (cond [(< len-a len-b) #t]
          [(> len-a len-b) #f]
          [else (LT a b)])))

(define (zeck> y z)
  (not (or (zeck= y z) (zeck< y z))))


;; Examples
;;
(define (example op-name op a b)
  (let* ([y (natural->zeck a)]
         [z (natural->zeck b)]
         [x (op y z)]
         [c (zeck->natural x)])
    (printf "~a ~a ~a = ~a ~a ~a = ~a = ~a\n"
            a op-name b y op-name z x c)))

(example '+ zeck+ 888 111)
(example '- zeck- 888 111)
(example '* zeck* 8 111)
(example '/ zeck-quotient 9876 1000)
(example '% zeck-remainder 9876 1000)
