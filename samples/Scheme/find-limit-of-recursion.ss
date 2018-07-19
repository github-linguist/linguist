(define (recurse number)
  (begin (display number) (newline) (recurse (+ number 1))))

(recurse 1)
