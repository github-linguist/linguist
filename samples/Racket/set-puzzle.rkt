#lang racket

(struct card [bits name])

(define cards
  (for/list ([C '(red   green    purple )] [Ci '(#o0001 #o0002 #o0004)]
             #:when #t
             [S '(oval  squiggle diamond)] [Si '(#o0010 #o0020 #o0040)]
             #:when #t
             [N '(one   two      three  )] [Ni '(#o0100 #o0200 #o0400)]
             #:when #t
             [D '(solid open     striped)] [Di '(#o1000 #o2000 #o4000)])
    (card (bitwise-ior Ci Si Ni Di) (format "~a, ~a, ~a, ~a" C S N D))))

(define (nsubsets l n)
  (cond [(zero? n) '(())] [(null? l) '()]
        [else (append (for/list ([l2 (nsubsets (cdr l) (- n 1))])
                        (cons (car l) l2))
                      (nsubsets (cdr l) n))]))
(define (set? cards)
  (regexp-match? #rx"^[1247]*$"
                 (number->string (apply bitwise-ior (map card-bits cards)) 8)))

(define (deal C S)
  (define hand  (take (shuffle cards) C))
  (define 3sets (filter set? (nsubsets hand 3)))
  (cond [(not (= S (length 3sets))) (deal C S)]
        [else (printf "Dealt ~a cards:\n" C)
              (for ([c hand]) (printf "  ~a\n" (card-name c)))
              (printf "\nContaining ~a sets:\n" S)
              (for ([set 3sets])
                (for ([c set]) (printf "  ~a\n" (card-name c)))
                (newline))]))

(deal 9 4)
(deal 12 6)
