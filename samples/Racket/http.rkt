#lang racket
(require net/url)
(copy-port (get-pure-port (string->url "http://www.rosettacode.org")
                          #:redirections 100)
           (current-output-port))
