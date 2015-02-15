; Commented line below should be uncommented to use read-line with Guile
;(use-modules (ice-9 rdelim))

(define file (open-input-file "input.txt"))
(do ((line (read-line file) (read-line file))) ((eof-object? line))
        (display line)
        (newline))
