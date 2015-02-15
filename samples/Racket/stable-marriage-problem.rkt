#lang racket

(define MEN
  '([abe  abi  eve  cath ivy  jan  dee  fay  bea  hope gay ]
    [bob  cath hope abi  dee  eve  fay  bea  jan  ivy  gay ]
    [col  hope eve  abi  dee  bea  fay  ivy  gay  cath jan ]
    [dan  ivy  fay  dee  gay  hope eve  jan  bea  cath abi ]
    [ed   jan  dee  bea  cath fay  eve  abi  ivy  hope gay ]
    [fred bea  abi  dee  gay  eve  ivy  cath jan  hope fay ]
    [gav  gay  eve  ivy  bea  cath abi  dee  hope jan  fay ]
    [hal  abi  eve  hope fay  ivy  cath jan  bea  gay  dee ]
    [ian  hope cath dee  gay  bea  abi  fay  ivy  jan  eve ]
    [jon  abi  fay  jan  gay  eve  bea  dee  cath ivy  hope]))
(define WOMEN
  '([abi  bob  fred jon  gav  ian  abe  dan  ed   col  hal ]
    [bea  bob  abe  col  fred gav  dan  ian  ed   jon  hal ]
    [cath fred bob  ed   gav  hal  col  ian  abe  dan  jon ]
    [dee  fred jon  col  abe  ian  hal  gav  dan  bob  ed  ]
    [eve  jon  hal  fred dan  abe  gav  col  ed   ian  bob ]
    [fay  bob  abe  ed   ian  jon  dan  fred gav  col  hal ]
    [gay  jon  gav  hal  fred bob  abe  col  ed   dan  ian ]
    [hope gav  jon  bob  abe  ian  dan  hal  ed   col  fred]
    [ivy  ian  col  hal  gav  fred bob  abe  ed   jon  dan ]
    [jan  ed   hal  gav  abe  bob  jon  col  ian  fred dan ]))

;; x is better than y according to the l order
(define (better? x y l) (memq y (cdr (memq x l))))

(define (stable-matching Mprefs Wprefs)
  (define M (map car Mprefs))
  (define engagements (make-hasheq))
  (define preferences (make-hasheq))
  (define (engage! m w)
    (hash-set! engagements m w)
    (hash-set! engagements w m))
  (for ([m Mprefs]) (hash-set! preferences (car m) (cdr m)))
  (for ([w Wprefs]) (hash-set! preferences (car w) (cdr w)))
  (let loop ()
    (define m+w
      (for/or ([m M])
        (and (not (hash-ref engagements m #f))   ; m is free
             (let ([p (hash-ref preferences m)]) ; can propose
               (and (pair? p)
                    (let ([w (car p)])
                      (hash-set! preferences m (cdr p)) ; drop w from m
                      (cons m w)))))))
    (when m+w
      (define m (car m+w))
      (define w (cdr m+w))
      (define m* (hash-ref engagements w #f))    ; m* is w's prev engagement
      (cond [(not m*) (engage! m w)]             ; w is free
            [(better? m m* (hash-ref preferences w)) ; w prefers m over m*
             (engage! m w)
             (hash-set! engagements m* #f)])     ; m* becomes free
      (loop)))
  engagements)

(define (find-unstable Mprefs Wprefs matches)
  (for*/or ([m (map car Mprefs)] [w (map car Wprefs)])
    (define w* (hash-ref matches m))
    (define m* (hash-ref matches w))
    (and (not (eq? m m*))
         (better? w w* (cdr (assq m Mprefs)))
         (better? m m* (cdr (assq w Wprefs)))
         (cons m w))))

(define (check-stability)
  (let ([u (find-unstable MEN WOMEN matches)])
    (if u
      (printf "Unstable: ~a and ~a prefer each other over partners.\n"
              (car u) (cdr u))
      (printf "The match is stable.\n"))))
(define matches (stable-matching MEN WOMEN))
(printf "Found matches:\n")
(for ([m (map car MEN)]) (printf "  ~a, ~a\n" m (hash-ref matches m)))
(check-stability)
(let ([M (map car (take (shuffle MEN) 2))])
  (printf "Swapping wives of ~a and ~a\n" (car M) (cadr M))
  (define (swap! x y)
    (define t (hash-ref matches x))
    (hash-set! matches x (hash-ref matches y))
    (hash-set! matches y t))
  (swap! (car M) (cadr M))
  (swap! (hash-ref matches (car M)) (hash-ref matches (cadr M))))
(check-stability)
