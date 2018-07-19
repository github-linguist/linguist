#lang racket
; utilities
(define-syntax def (make-rename-transformer #'define))
(define (dict-ref d w) (hash-ref d w #f))
(define (append-char w c)  (string-append w (string c)))
(define (append-first w s) (append-char w (string-ref s 0)))

;; Compress a string with LZW
(define (compress uncompressed)
  (def d (make-hash))
  (def (dict-add d w) (hash-set! d w (hash-count d)))
  ; build initial dictionary
  (for ([i (in-range 256)])
    (def s (string (integer->char i)))
    (hash-set! d s s))
  ; compress the string
  (def result '())
  (def (emit! i) (set! result (cons i result)))
  (def w "")
  (for ([c uncompressed])
    (define wc (append-char w c))
    (cond
      [(dict-ref d wc) (set! w wc)]
      [else            (emit! (dict-ref d w))
                       (dict-add d wc)
                       (set! w (string c))]))
  (emit! (dict-ref d w))
  (reverse result))

;; Decompress a LZW compressed string
(define (decompress compressed)
  (def d (make-hash))
  (def (dict-add! w) (hash-set! d (hash-count d) w))
  ; build initial dictionary
  (for ([i (in-range 256)])
    (def s (string (integer->char i)))
    (hash-set! d s s))
  ; decompress the list
  (def w (first compressed))
  (apply string-append
         w
         (for/list ([k (rest compressed)])
           (def entry
             (or (dict-ref d k)
                 (if (eqv? k (hash-count d))
                     (append-first w w)
                     (error 'lzq-decompress "faulty input"))))
           (dict-add! (append-first w entry))
           (set! w entry)
           entry)))

(def uncompressed "TOBEORNOTTOBEORTOBEORNOT")
(displayln uncompressed)
(def compressed (compress uncompressed))
(displayln compressed)
(def decompressed (decompress compressed))
(displayln decompressed)
