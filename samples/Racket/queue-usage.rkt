#lang racket

(require data/queue)

(define queue (make-queue))

(enqueue! queue 'black)
(queue-empty? queue) ; #f

(enqueue! queue 'red)
(enqueue! queue 'green)

(dequeue! queue) ; 'black
(dequeue! queue) ; 'red
(dequeue! queue) ; 'green

(queue-empty? queue) ; #t
