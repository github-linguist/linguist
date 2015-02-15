-> ((lambda (x) (equal? (read) (list x (list 'quote x))))
   '(lambda (x) (equal? (read) (list x (list 'quote x)))))
((lambda (x) (equal? (read) (list x (list 'quote x))))
 '(lambda (x) (equal? (read) (list x (list 'quote x)))))
#t
