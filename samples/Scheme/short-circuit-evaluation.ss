>(define (a x)
   (display "a\n")
   x)
>(define (b x)
   (display "b\n")
   x)
>(for-each (lambda (i)
   (for-each (lambda (j)
     (display i) (display " and ") (display j) (newline)
     (and (a i) (b j))
     (display i) (display " or ") (display j) (newline)
     (or (a i) (b j))
    ) '(#t #f))
  ) '(#t #f))
#t and #t
a
b
#t or #t
a
#t and #f
a
b
#t or #f
a
#f and #t
a
#f or #t
a
b
#f and #f
a
#f or #f
a
b
