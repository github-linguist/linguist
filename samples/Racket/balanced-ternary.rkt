#lang racket

;; Represent a balanced-ternary number as a list of 0's, 1's and -1's.
;;
;; e.g. 11 = 3^2 + 3^1 - 3^0 ~ "++-" ~ '(-1 1 1)
;;       6 = 3^2 - 3^1       ~ "+-0" ~ '(0 -1 1)
;;
;; Note: the list-rep starts with the least signifcant tert, while
;;       the string-rep starts with the most significsnt tert.

(define (bt->integer t)
  (if (null? t)
      0
      (+ (first t) (* 3 (bt->integer (rest t))))))

(define (integer->bt n)
  (letrec ([recur (λ (b r) (cons b (convert (floor (/ r 3)))))]
           [convert (λ (n) (if (zero? n) null
                               (case (modulo n 3)
                                 [(0) (recur 0 n)]
                                 [(1) (recur 1 n)]
                                 [(2) (recur -1 (add1 n))])))])
    (convert n)))

(define (bt->string t)
  (define (strip-leading-zeroes a)
    (if (or (null? a) (not (= (first a) 0))) a (strip-leading-zeroes (rest a))))
  (string-join (map (λ (u)
                      (case u
                        [(1) "+"]
                        [(-1) "-"]
                        [(0) "0"]))
                    (strip-leading-zeroes (reverse t))) ""))

(define (string->bt s)
  (reverse
   (map (λ (c)
          (case c
            [(#\+) 1]
            [(#\-) -1]
            [(#\0) 0]))
        (string->list s))))

(define (bt-negate t)
  (map (λ (u) (- u)) t))

(define (bt-add a b [c 0])
  (cond [(and (null? a) (null? b)) (if (zero? c) null (list c))]
        [(null? b) (if (zero? c) a (bt-add a (list c)))]
        [(null? a) (bt-add b a c)]
        [else (let* ([t (+ (first a) (first b) c)]
                     [carry (if (> (abs t) 1) (sgn t) 0)]
                     [v (case (abs t)
                          [(3) 0]
                          [(2) (- (sgn t))]
                          [else t])])
                (cons v (bt-add (rest a) (rest b) carry)))]))

(define (bt-multiply a b)
  (cond [(null? a) null]
        [(null? b) null]
        [else (bt-add (case (first a)
                        [(-1) (bt-negate b)]
                        [(0) null]
                        [(1) b])
                      (cons 0 (bt-multiply (rest a) b)))]))

; test case
(let* ([a (string->bt "+-0++0+")]
       [b (integer->bt -436)]
       [c (string->bt "+-++-")]
       [d (bt-multiply a (bt-add b (bt-negate c)))])
  (for ([bt (list a b c d)]
        [description (list 'a 'b 'c "a×(b−c)")])
    (printf "~a = ~a or ~a\n" description (bt->integer bt) (bt->string bt))))
