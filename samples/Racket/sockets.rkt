#lang racket
(let-values ([(in out) (tcp-connect "localhost" 256)])
  (display "hello socket world\n" out)
  (close-output-port out))
