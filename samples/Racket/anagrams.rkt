#lang racket

(require net/url)

(define (get-lines url-string)
  (define port (get-pure-port (string->url url-string)))
  (for/list ([l (in-lines port)]) l))

(define (hash-words words)
  (for/fold ([ws-hash (hash)]) ([w words])
    (hash-update ws-hash
                 (list->string (sort (string->list w) < #:key (λ (c) (char->integer c))))
                 (λ (ws) (cons w ws))
                 (λ () '()))))

(define (get-maxes h)
  (define max-ws (apply max (map length (hash-values h))))
  (define max-keys (filter (λ (k) (= (length (hash-ref h k)) max-ws)) (hash-keys h)))
  (map (λ (k) (hash-ref h k)) max-keys))

(get-maxes (hash-words (get-lines "http://www.puzzlers.org/pub/wordlists/unixdict.txt")))
