;; The American Soundex System
;;
;; The soundex code consist of the first letter of the name followed
;; by three digits. These three digits are determined by dropping the
;; letters a, e, i, o, u, h, w and y and adding three digits from the
;; remaining letters of the name according to the table below. There
;; are only two additional rules. (1) If two or more consecutive
;; letters have the same code, they are coded as one letter. (2) If
;; there are an insufficient numbers of letters to make the three
;; digits, the remaining digits are set to zero.

;; Soundex Table

;;  1 b,f,p,v
;;  2 c,g,j,k,q,s,x,z
;;  3 d, t
;;  4 l
;;  5 m, n
;;  6 r

;; Examples:

;;  Miller M460
;;  Peterson P362
;;  Peters P362
;;  Auerbach A612
;;  Uhrbach U612
;;  Moskowitz M232
;;  Moskovitz M213

(define (char->soundex c)
  (case (char-upcase c)
    ((#\B #\F #\P #\V) #\1)
    ((#\C #\G #\J #\K #\Q #\S #\X #\Z) #\2)
    ((#\D #\T) #\3)
    ((#\L) #\4)
    ((#\M #\N) #\5)
    ((#\R) #\6)
    (else #\nul)))

(define (collapse-dups lst)
  (if (= (length lst) 1) lst
      (if (equal? (car lst) (cadr lst))
	  (collapse-dups (cdr lst))
	  (cons (car lst) (collapse-dups (cdr lst))))))

(define (remove-nul lst)
  (filter (lambda (c)
	    (not (equal? c #\nul)))
	  lst))

(define (force-len n lst)
  (cond ((= n 0) '())
	((null? lst) (force-len n (list #\0)))
	(else (cons (car lst) (force-len (- n 1) (cdr lst))))))

(define (soundex s)
  (let ((slst (string->list s)))
    (force-len 4 (cons (char-upcase (car slst))
		       (remove-nul
			(collapse-dups
			 (map char->soundex (cdr slst))))))))

(soundex "miller")
(soundex "Peterson")
(soundex "PETERS")
(soundex "auerbach")
(soundex "Uhrbach")
(soundex "Moskowitz")
(soundex "Moskovitz")
