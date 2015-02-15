#!/usr/bin/env racket
#lang racket

(define (program) (find-system-path 'run-file))

(module+ main (printf "Program: ~a\n" (program)))
