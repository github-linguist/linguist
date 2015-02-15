#lang racket/base
(require ffi/unsafe)

(provide strdup)

;; Helper: create a Racket string from a C string pointer.
(define make-byte-string
  (get-ffi-obj "scheme_make_byte_string" #f (_fun _pointer -> _scheme)))

;; Take special care not to allow NULL (#f) to be passed as an input,
;; as that will crash strdup.
(define _string/no-null
  (make-ctype _pointer
    (lambda (x)
      (unless (string? x)
        (raise-argument-error '_string/no-null "string" x))
      (string->bytes/utf-8 x))
    ;; We don't use _string/no-null as an output type, so don't care:
    (lambda (x) x)))

; Make a Scheme string from the C string, and free immediately.
(define _string/free
  (make-ctype _pointer
    ;; We don't use this as an input type, so we don't care.
    (lambda (x) x)
    (lambda (x)
      (cond
       [x
        (define s (bytes->string/utf-8 (make-byte-string x)))
        (free x)
        s]
       [else
        ;; We should never get null from strdup unless we're out of
        ;; memory:
        (error 'string/free "Out of memory")]))))

(define strdup
  (get-ffi-obj "strdup" #f (_fun _string/no-null -> _string/free)))

;; Let's try it:
(strdup "Hello World!")
