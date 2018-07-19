(define-syntax if2
  (syntax-rules ()
    ((if2 cond1 cond2 both-true first-true second-true none-true)
     (let ((c2 cond2))
       (if cond1
           (if c2 both-true first-true)
           (if c2 second-true none-true))))))
