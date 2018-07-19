#lang racket
; if we `for` over the port, we won't have the program in memory for 'Q'
(define (parse-HQ9+ the-program)
  (define oTW " on the wall")
  (and ; ensures the accumulator is never seen!
   (for/fold ((A 0))
     ((token (in-string the-program)))
     (case token
       ((#\H) (display "hello, world") A)
       ((#\Q) (display the-program) A)
       ;; official esolang version of 99-BoB at:
       ;;  http://esolangs.org/wiki/99_bottles_of_beer
       ((#\9)
        (displayln
        (let ((BoB (lambda (n)
                     (string-append
                      (case n ((1) "1 bottle") ((0) "No bottles")
                        (else (format "~a bottles" n)))
                      " of beer"))))
          (string-join
           (for/list ((btls (in-range 99 0 -1)))
            (string-append (BoB btls)oTW",\n"(BoB btls)
                           ".\nTake one down, pass it around,\n"
                           (BoB (sub1 btls))oTW"."))
           "\n\n"))) A)
       ((#\+) (add1 A))
       ((#\newline) ; language extension, makes getting standard in easier
        (eprintf "warning: HQ9+: language extension ~s" token)
        A)
       (else (error "syntax error: HQ9+: unrecognised token ~s" token))))
   (void)))

(module+ main (parse-HQ9+ (port->string)))

(module+ test
  (require rackunit)
  (check-equal? (with-output-to-string (lambda () (parse-HQ9+ ""))) "")
  (check-equal? (with-output-to-string (lambda () (parse-HQ9+ "H"))) "hello, world")
  (check-equal? (with-output-to-string (lambda () (parse-HQ9+ "Q"))) "Q")
  (check-equal? (with-output-to-string (lambda () (parse-HQ9+ "QQ"))) "QQQQ")
  (check-equal? (with-output-to-string (lambda () (parse-HQ9+ "+"))) "")
  (check-equal? (with-output-to-string (lambda () (parse-HQ9+ "+"))) "")
  (check-equal? (with-output-to-string (lambda () (parse-HQ9+ "++"))) "")
  (check-equal? (with-output-to-string (lambda () (parse-HQ9+ "+++"))) "")
  (check-equal? (with-output-to-string (lambda () (parse-HQ9+ "+++++++++++++++++"))) "")
  (check-equal? (with-output-to-string (lambda () (parse-HQ9+ (make-string 10000 #\+)))) "")
  ;;; you can jolly well read (and sing along to) the output of '9'
  )
