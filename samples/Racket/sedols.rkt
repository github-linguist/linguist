#lang racket
;;; Since the Task gives us unchecksummed and checksummed SEDOLs, and
;;; we'll just take a list of the output SEDOLs and remove their last
;;; characters for the input
(define output-SEDOLS
  (list "7108899" "B0YBKJ7" "4065663"
        "B0YBLH2" "2282765" "B0YBKL9"
        "5579107" "B0YBKR5" "5852842"
        "B0YBKT7" "B000300"))
(define (output->input-SEDOL S) (substring S 0 6))
(define input-SEDOLS (map output->input-SEDOL output-SEDOLS))

;;; checksum calculation
(define (SEDOL-character-value c)
  (if (char-numeric? c)
      (- (char->integer c) (char->integer #\0))
      (+ 10 (- (char->integer c) (char->integer #\A)))))
(define (SEDOL-character-sum S)
  (for/sum ((c S)  ; if we run out of c's before the final 1 in weight, we'll have the unchecksummed weighted sum
            (weight (in-list '(1 3 1 7 3 9 1))))
    (* weight (SEDOL-character-value c))))
(define (SEDOL-checksum S) (number->string (modulo (- 10 (SEDOL-character-sum S)) 10)))

;;; build output from input
(define (SEDOL-append-checksum S) (string-append S (SEDOL-checksum S)))

;;; Extra credit -- according to wikipedia:
;;; "SEDOLs are seven characters in length"
;;; "vowels are never used"
;;; there seems to be no statement as to case, but we'll assert that too!
;;;
;;; valid-SEDOL? is a predicate... it doesn't report a reason
(define (invalid-SEDOL? S)
  (define (invalid-SEDOL-character? c)
    (if
     (and (not (char-upper-case? c)) (not (char-numeric? c)))
     (format "contains non upper case/non numeric ~a" c)
     (case c [(#\A #\E #\I #\O #\U) (format "contains vowel ~a" c)] [else #f])))
  (cond
    [(< (string-length S) 7) "too few characters"]
    [(> (string-length S) 7) "too many characters"]
    [(not (zero? (modulo (SEDOL-character-sum S) 10))) "invalid checksum"]
    [(for/first ((c S) #:when (invalid-SEDOL-character? c)) c) => identity]
    [else #f])) ; a.k.a. valid!

(module+ main
  (for* ((S input-SEDOLS))
    (displayln (SEDOL-append-checksum S)))
  (newline)
  (displayln "Extra Credit!")
  (displayln (invalid-SEDOL? "B0YBKT7")) ; expect #f output
  (displayln (invalid-SEDOL? "B000301")) ; expect "invalid checksum" output
  )

(module+ test
  (require rackunit)
  (check-= (SEDOL-character-value #\3) 3 0)
  (check-= (SEDOL-character-value #\B) 11 0)
  (check-equal? (invalid-SEDOL? "B000301") "invalid checksum")
  (for ((S output-SEDOLS))
    (check-false (invalid-SEDOL? S))
    (check-equal? (SEDOL-append-checksum (substring S 0 6))
                  S (format "test SEDOL for ~a" S))))
