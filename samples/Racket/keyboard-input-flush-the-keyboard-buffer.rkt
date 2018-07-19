#lang racket
(define-syntax-rule (with-raw body ...)
  (let ([saved #f])
    (define (stty x) (system (~a "stty " x)) (void))
    (dynamic-wind (λ() (set! saved (with-output-to-string (λ() (stty "-g"))))
                       (stty "raw -echo opost"))
                  (λ() body ...)
                  (λ() (stty saved)))))

(with-raw
  (printf "Keys pressed from now will be ignored\n")
  (sleep 2)
  (let loop () (when (char-ready?) (read-char) (loop))) ; flush input
  (printf "Now press a key which will not be ignored\n")
  (printf "You pressed ~a\n" (read-char)))
