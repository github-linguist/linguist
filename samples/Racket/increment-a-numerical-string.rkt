#lang racket
(define next (compose number->string add1 string->number))
