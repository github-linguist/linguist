#lang racket
(with-output-to-file "/dev/tape" #:exists 'append
  (λ() (displayln "I am a cheap imitation of the Perl code for a boring problem")))
