#!/bin/sh
#|
exec csi -ss $0 ${1+"$@"}
exit
|#

(use posix)
(require-extension srfi-1) ; lists
(require-extension srfi-13) ; strings

(define (main args)
	(let ((prog (cdr (program))))
		(display (format "Program: ~a\n" prog))
		(exit)))

(define (program)
	(if (string=? (car (argv)) "csi")
		(let ((s-index (list-index (lambda (x) (string-contains x "-s")) (argv))))
			(if (number? s-index)
				(cons 'interpreted (list-ref (argv) (+ 1 s-index)))
				(cons 'unknown "")))
		(cons 'compiled (car (argv)))))

(if (equal? (car (program)) 'compiled)
	(main (cdr (argv))))
