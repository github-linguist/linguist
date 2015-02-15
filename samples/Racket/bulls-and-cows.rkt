#lang racket

; secret : (listof exact-nonnegative-integer?)
(define secret
  (foldr (λ (n result)
           (cons n (map (λ (y) (if (>= y n) (add1 y) y))
                        result)))
         '()
         (map random '(10 9 8 7))))

; (count-bulls/cows guess) -> (values exact-nonnegative-integer?
;                                     exact-nonnegative-integer?)
; guess : (listof exact-nonnegative-integer?)
(define (count-bulls/cows guess)
  (let* ([bulls (map = guess secret)]
         [cow-candidates (filter-map (λ (x y) (if (false? x) y #f))
                                      bulls
                                      secret)]
         [cows (filter (curryr member cow-candidates) guess)])
    (values (length (filter ((curry equal?) #t) bulls))
            (length cows))))

; (valid-guess guess-str) -> (or/c (listof exact-nonnegative-integer?) #f)
; guess-str : string?
(define (valid-guess guess-str)
  (define (char->digit c)
    (- (char->integer c) (char->integer #\0)))
  (if (regexp-match-exact? #px"[0-9]{4}" guess-str)
      (let ([guess (map char->digit (string->list guess-str))])
        (if (andmap (λ (x) (equal? (count ((curry equal?) x) guess) 1))
                    guess)
            guess
            #f))
      #f))

; Game states
(define win #t)
(define game #f)

; (main-loop state step) -> void?
; state : boolean?
; step  : exact-nonnegative-integer?
(define (main-loop state step)
  (if (equal? state win)
      (printf "You won after ~a guesses." step)
      (begin
        (let* ([guess-str (read-line)]
               [guess (valid-guess guess-str)])
          (if (false? guess)
              (begin (displayln "Guess should include exactly four different digits")
                     (main-loop state step))
              (let-values ([(bulls cows) (count-bulls/cows guess)])
                (if (= bulls 4)
                    (main-loop win (add1 step))
                    (begin (printf "Bulls: ~a Cows: ~a\n" bulls cows)
                           (main-loop state (add1 step))))))))))

(main-loop game 0)
