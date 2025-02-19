;; Calculation of Hofstadter's male and female sequences as a list of pairs

(define (hofstadter-male-female n)
  (letrec ((female (lambda (n)
		     (if (= n 0)
			 1
			 (- n (male (female (- n 1)))))))
	   (male (lambda (n)
		   (if (= n 0)
		       0
		       (- n (female (male (- n 1))))))))
    (let loop ((i 0))
      (if (> i n)
	  '()
	  (cons (cons (female i)
		      (male i))
		(loop (+ i 1)))))))

(hofstadter-male-female 8)

;; Building a list of squares from 0 to 9:
;; Note: loop is simply an arbitrary symbol used as a label. Any symbol will do.

(define (list-of-squares n)
  (let loop ((i n) (res '()))
    (if (< i 0)
        res
        (loop (- i 1) (cons (* i i) res)))))

(list-of-squares 9)

(let loop ((n 1))
  (if (> n 10)
      '()
      (cons n
	    (loop (+ n 1)))))

(define (find-first func lst)
  (call-with-current-continuation
   (lambda (return-immediately)
     (for-each (lambda (x)
		 (if (func x)
		     (return-immediately x)))
	  lst)
     #f)))

(find-first integer? '(1/2 3/4 5.6 7 8/9 10 11))


(set! +
      (let ((original+ +))
        (lambda args
          (apply (if (or (null? args) (string? (car args)))
                     string-append
                     original+)
                 args))))
(+ 1 2 3)

; create a hash-table, with keys :a and :b
(define my-hash (hash-table :a 1 :b 2)

(map
  (lambda (x y) (list x y))
  (list :a :b :c)   ; first collection
  (list 1 2 3))     ; second collection6

;; example using break and continue with random,
;; a construct we might use in a musical algorithm
(let ((count 0))
  ; execute a maxium of 8 times
  (while (< count 8)
    (post "count:" count)

    ; a 1 in 4 chance we quit after each time
    (if (= 0 (random 4))
      (begin (post "aborting!") (break)))

    ; a 1 in 4 chance we stay on the same number
    (if (= 0 (random 3))
      (begin (post "continuing!") (continue)))

    (set! count (+ 1 count))))

; for-each will execute the functions, return #<unspecified>
(for-each
  (lambda (x) (post "loop pass, x is:" x))
  (range 0 4))

