#lang racket
(require net/url)
(copy-port (get-pure-port (string->url "https://www.google.com")
                          #:redirections 100)
           (current-output-port))
