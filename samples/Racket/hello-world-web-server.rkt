#lang racket
(require web-server/servlet web-server/servlet-env)
(define (start req) (response/xexpr "Goodbye, World!"))
(serve/servlet start #:port 8080 #:servlet-path "/")
