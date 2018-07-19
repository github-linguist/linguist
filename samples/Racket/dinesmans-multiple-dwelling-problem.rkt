#lang racket

;; A quick `amb' implementation
(define fails '())
(define (fail) (if (pair? fails) ((car fails)) (error "no more choices!")))
(define (amb xs)
  (let/cc k (set! fails (cons k fails)))
  (if (pair? xs) (begin0 (car xs) (set! xs (cdr xs)))
      (begin (set! fails (cdr fails)) (fail))))
(define (assert . conditions) (when (memq #f conditions) (fail)))

;; Convenient macro for definining problem items
(define-syntax-rule (with: all (name ...) #:in choices body ...)
  (let* ([cs choices] [name (amb cs)] ... [all `([,name name] ...)]) body ...))

;; ===== problem translation starts here =====

;; Baker, Cooper, Fletcher, Miller, and Smith live on different floors
;; of an apartment house that contains only five floors.
(with: residents [Baker Cooper Fletcher Miller Smith] #:in (range 1 6)
  ;; Some helpers
  (define (on-top    x) (for/and ([y residents]) (x . >= . (car y))))
  (define (on-bottom x) (for/and ([y residents]) (x . <= . (car y))))
  (define (adjacent x y) (= 1 (abs (- x y))))
  (assert
   ;; ... live on different floors ...
   (assert (= 5 (length (remove-duplicates (map car residents)))))
   ;; Baker does not live on the top floor.
   (not (on-top Baker))
   ;; Cooper does not live on the bottom floor.
   (not (on-bottom Cooper))
   ;; Fletcher does not live on either the top or the bottom floor.
   (not (on-top Fletcher))
   (not (on-bottom Fletcher))
   ;; Miller lives on a higher floor than does Cooper.
   (> Miller Cooper)
   ;; Smith does not live on a floor adjacent to Fletcher's.
   (not (adjacent Smith Fletcher))
   ;; Fletcher does not live on a floor adjacent to Cooper's.
   (assert (not (adjacent Fletcher Cooper))))
  ;; Where does everyone live?
  (printf "Solution:\n")
  (for ([x (sort residents > #:key car)]) (apply printf "  ~a. ~a\n" x)))
