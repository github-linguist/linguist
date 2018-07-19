#lang racket
(define seen (make-hash))
(define semordnilaps '())
(call-with-input-file "/usr/share/dict/words"
  (λ(i) (for ([l (in-lines i)])
          (define r (list->string (reverse (string->list l))))
          (unless (equal? r l)
            (hash-set! seen l #t)
            (when (hash-ref seen r #f)
              (set! semordnilaps (cons (list r l) semordnilaps)))))))
(printf "Total semordnilaps found: ~s\n" (length semordnilaps))
(printf "The five longest ones:\n")
(for ([s (take (sort semordnilaps > #:key (compose1 string-length car)) 5)])
  (apply printf "  ~s ~s\n" s))
