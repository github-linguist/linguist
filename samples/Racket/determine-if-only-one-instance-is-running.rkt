#lang racket
(define *port* 12345) ; random large port number
(define listener-handler
  (with-handlers ([exn? (λ(e) (printf "Already running, bye.\n") (exit))])
    (tcp-listen *port*)))
(printf "Working...\n")
(sleep 10)
