#lang racket
(define-syntax-rule (with-raw body ...)
  (let ([saved #f])
    (define (stty x) (system (~a "stty " x)) (void))
    (dynamic-wind (λ() (set! saved (with-output-to-string (λ() (stty "-g"))))
                       (stty "raw -echo opost"))
                  (λ() body ...)
                  (λ() (stty saved)))))

(with-raw
  (printf "Press a key, or not\n")
  (sleep 2)
  (if (char-ready?)
    (printf "You pressed ~a\n" (read-char))
    (printf "You didn't press a key\n")))
