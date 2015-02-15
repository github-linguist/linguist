=> (define (create-variable name initial-val)
     (eval `(define ,name ,initial-val) (interaction-environment)))

=> (create-variable (read) 50)
<hello

=> hello
50
