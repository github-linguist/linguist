#!/usr/bin/env newlisp

(constant 'NUM 8)

(define (intersects? q1 q2)
	(or 
		(= (q1 0) (q2 0)) 
		(= (q1 1) (q2 1))
		(= (abs (- (q1 0) (q2 0))) (abs (- (q1 1) (q2 1))))))

(define (variant? alist)
	(set 'logic nil)
	(cond
		((= (length alist) 1) true)
		((> (length alist) 1)
			(while (> (length alist) 1)
				(set 'q (pop alist -1))
				(dolist (el alist)
					(push 
						(intersects? 
							(list q (inc (length alist)))
							(list el (+ 1 $idx)))
					logic -1)))
			(not (apply or logic)))))

(define (fork-by-line alist)
	(let (res '())
		(dolist (i (sequence 1 NUM))
			(set 'tmp alist)
			(push i tmp -1)
			(setf res (push tmp res -1)))
		res))

(define (find-variants num)
	(let (res '())
		(cond 
			((< num 1) 
				(begin (println "num < 1") (exit)))
			((= num 1) 
				(dolist (i (sequence 1 NUM)) (push (list i) res -1)))
			((> num 1) 
				(dolist (v (find-variants (dec num))) 
					(set 'passed (filter variant? (fork-by-line v)))
					(if (not (empty? passed)) (extend res passed)))))
		res))
		
(set 'solutions (find-variants NUM))
(println (length solutions))
;;(exit)