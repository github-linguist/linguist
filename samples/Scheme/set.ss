(define (element? a lst)
  (and (not (null? lst))
       (or (eq? a (car lst))
	   (element? a (cdr lst)))))

; util, not strictly needed
(define (uniq lst)
  (if (null? lst) lst
    (let ((a (car lst)) (b (cdr lst)))
      (if (element? a b)
	(uniq b)
	(cons a (uniq b))))))

(define (intersection a b)
  (cond ((null? a) '())
	((null? b) '())
	(else
	  (append (intersection (cdr a) b)
		  (if (element? (car a) b)
		    (list (car a))
		    '())))))

(define (union a b)
  (if (null? a) b
    (union (cdr a)
	   (if (element? (car a) b)
	     b
	     (cons (car a) b)))))

(define (diff a b) ; a - b
  (if (null? a) '()
    (if (element? (car a) b)
      (diff (cdr a) b)
      (cons (car a) (diff (cdr a) b)))))

(define (subset? a b) ; A ⊆ B
  (if (null? a) #t
    (and (element? (car a) b)
	 (subset? (cdr a) b))))

(define (set-eq? a b)
  (and (subset? a b)
       (subset? b a)))
