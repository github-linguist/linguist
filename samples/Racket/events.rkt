#lang racket

(define task (thread (lambda () (printf "Got: ~s\n" (thread-receive)))))

(thread-send task ; wait for it, then send it
             (sync (alarm-evt (+ 1000 (current-inexact-milliseconds)))))

(void (sync task)) ; wait for the task to be done before exiting
