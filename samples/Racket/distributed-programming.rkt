#lang racket/base
(require racket/place/distributed racket/place)

(define (fib n)
  (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))

(provide work)
(define (work)
  (place ch
    (place-channel-put ch (fib (place-channel-get ch)))))

(module+ main
  (define places
    (for/list ([host '("localhost" "localhost" "localhost" "localhost")]
               [port (in-naturals 12345)])
      (define-values [node place]
        (spawn-node-supervise-place-at host #:listen-port port #:thunk #t
                                       (quote-module-path "..") 'work))
      place))
  (message-router
   (after-seconds 1
     (for ([p places]) (*channel-put p 42))
     (printf "Results: ~s\n" (map *channel-get places))
     (exit))))
