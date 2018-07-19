#lang racket
(define word-list-file "data/unixdict.txt")

(define (read-words-into-anagram-keyed-hash)
  (define (anagram-key word) (sort (string->list word) char<?))
  (for/fold ((hsh (hash)))
    ((word (in-lines)))
    (hash-update hsh (anagram-key word) (curry cons word) null)))

(define anagrams-list
  (sort
   (for/list
       ((v (in-hash-values
            (with-input-from-file
                word-list-file
              read-words-into-anagram-keyed-hash)))
        #:when (> (length v) 1)) v)
   > #:key (compose string-length first)))


(define (deranged-anagram-pairs l (acc null))
  (define (deranged-anagram-pair? hd tl)
    (define (first-underanged-char? hd tl)
      (for/first
          (((c h) (in-parallel hd tl))
           #:when (char=? c h)) c))
    (not (first-underanged-char? hd tl)))

  (if (null? l) acc
      (let ((hd (car l)) (tl (cdr l)))
        (deranged-anagram-pairs
         tl
         (append acc (map (lambda (x) (list hd x))
                          (filter (curry deranged-anagram-pair? hd) tl)))))))

;; for*/first give the first set of deranged anagrams (as per the RC problem)
;; for*/list gives a full list of the sets of deranged anagrams (which might be interesting)
(for*/first
    ((anagrams (in-list anagrams-list))
     (daps (in-value (deranged-anagram-pairs anagrams)))
     #:unless (null? daps))
  daps)
