#lang racket
(require net/url)

(define dict "http://www.puzzlers.org/pub/wordlists/unixdict.txt")

(define (ordered? str)
  (define lower (string-downcase str))
  (for/and ([i (in-range 1 (string-length str))])
    (char<=? (string-ref lower (sub1 i)) (string-ref lower i))))

(define words (port->lines (get-pure-port (string->url dict))))

(let loop ([len 0] [longs '()] [words words])
  (if (null? words)
    (for-each displayln (reverse longs))
    (let* ([word (car words)] [words (cdr words)]
           [wlen (string-length word)])
      (if (or (< wlen len) (not (ordered? word)))
        (loop len longs words)
        (loop wlen (cons word (if (> wlen len) '() longs)) words)))))
