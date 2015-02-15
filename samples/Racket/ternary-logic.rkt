#lang typed/racket

; to avoid the hassle of adding a maybe value that is as special as
; the two standard booleans, we'll use symbols to make our own
(define-type trit (U 'true 'false 'maybe))

(: not (trit -> trit))
(define (not a)
  (case a
    [(true) 'false]
    [(maybe) 'maybe]
    [(false) 'true]))

(: and (trit trit -> trit))
(define (and a b)
  (case a
    [(false) 'false]
    [(maybe) (case b
               [(false) 'false]
               [else 'maybe])]
    [(true) (case b
              [(true) 'true]
              [(maybe) 'maybe]
              [(false) 'false])]))

(: or (trit trit -> trit))
(define (or a b)
  (case a
    [(true) 'true]
    [(maybe) (case b
               [(true) 'true]
               [else 'maybe])]
    [(false) (case b
               [(true) 'true]
               [(maybe) 'maybe]
               [(false) 'false])]))

(: ifthen (trit trit -> trit))
(define (ifthen a b)
  (case b
    [(true) 'true]
    [(maybe) (case a
               [(false) 'true]
               [else 'maybe])]
    [(false) (case a
               [(true) 'false]
               [(maybe) 'maybe]
               [(false) 'true])]))

(: iff (trit trit -> trit))
(define (iff a b)
  (case a
    [(maybe) 'maybe]
    [(true) b]
    [(false) (not b)]))

(for: : Void ([a (in-list '(true maybe false))])
      (printf "~a ~a = ~a~n" (object-name not) a (not a)))
(for: : Void ([proc (in-list (list and or ifthen iff))])
  (for*: : Void ([a (in-list '(true maybe false))]
                 [b (in-list '(true maybe false))])
         (printf "~a ~a ~a = ~a~n" a (object-name proc) b (proc a b))))
