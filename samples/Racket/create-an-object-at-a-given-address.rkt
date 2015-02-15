#lang racket
(require ffi/unsafe)

(define x #"Foo")
;; Get the address of the `x' object
(printf "The address of `x' is: ~s\n" (cast x _scheme _long))
(define address (cast x _bytes _long))
(printf "The address of the bytestring it holds: ~s\n" address)
(define y (cast address _long _bytes))
(printf "Converting this back to a bytestring: ~s\n" y)
(bytes-set! y 0 71)
(printf "Changed the converted bytestring: ~s\n" y)
(printf "The original one is now: ~s\n" x)
;; But (bytes-set! x 0 71) will throw an error since `x' is immutable,
;; showing that we've really modifed the memory directly in a way that
;; the runtime doesn't like.

;; Also, the above can fail at any moment if a GC happens, since
;; Racket's GC moves objects.  So a proper way to do this is not to
;; start from an existing object, but allocate one outside of the GC's
;; reach, using raw malloc():
(define buf (malloc 4 'raw))
(make-sized-byte-string buf 4)
;; or start with a given address of something like a memory-mapped IO
;; object
