#lang racket
(require openssl/mzssl)
(define ctx (ssl-make-client-context))
(ssl-set-verify! ctx #t) ; verify the connection
(ssl-load-verify-root-certificates! ctx "my-cert.pem")
(define-values [I O] (ssl-connect "www.example.com" 443 ctx))
