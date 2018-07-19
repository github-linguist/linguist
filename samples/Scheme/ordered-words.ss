(define sorted-words
  (let ((port (open-input-file "unixdict.txt")))
    (let loop ((char (read-char port)) (word '()) (result '(())))
      (cond
       ((eof-object? char)
	(reverse (map (lambda (word) (apply string word)) result)))
       ((eq? #\newline char)
	(loop (read-char port) '()
	      (let ((best-length (length (car result))) (word-length (length word)))
		(cond
		 ((or (< word-length best-length) (not (apply char>=? word))) result)
		 ((> word-length best-length) (list (reverse word)))
		 (else (cons (reverse word) result))))))
       (else (loop (read-char port) (cons char word) result))))))

(map (lambda (x)
       (begin
	 (display x)
	 (newline)))
     sorted-words)
