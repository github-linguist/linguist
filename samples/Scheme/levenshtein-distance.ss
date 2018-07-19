#!/usr/local/bin/gosh

(define (main args)
    (let* ((a "kitten")
           (b "sitting")
           (d (levenshteinDistance a b)))
        (format #t "Distance between ~a and ~a is ~a.~%" a b d)))


;; Number of edits needed to turn s1 into s2.
(define (levenshteinDistance s1 s2)
    (let*  ((s1Len (string-length s1))
            (s2Len (string-length s2)))
        (cond
            ; If either empty, distance is length of the other. (insert all)
            ((= 0 s1Len) s2Len)
            ((= 0 s2Len) s1Len)
            (else
                (let* ((s1Chars (string->list s1))
                       (s2Chars (string->list s2))
                       (s1First (car s1Chars))
                       (s2First (car s2Chars))
                       (s1Rest  (list->string (cdr s1Chars)))
                       (s2Rest  (list->string (cdr s2Chars))))
                    (cond
                        ; If first chars equal, then use distance between the tails.
                        ((equal? s1First s2First)
                            (levenshteinDistance s1Rest s2Rest))
                        ; Distance is minimum of sub-strings.
                        (else (+ 1 (min (levenshteinDistance s1Rest s2)
                                        (levenshteinDistance s1 s2Rest)
                                        (levenshteinDistance s1Rest s2Rest))))))))))
