> (define x 37)
> (eval '(+ x 5))
42
> (eval '(+ x 5) (interaction-environment))
42
> (eval '(+ x 5) (scheme-report-environment 5)) ;; provides R5RS definitions

Error: identifier not visible x.
Type (debug) to enter the debugger.
> (display (eval (read)))
(+ 4 5) ;; this is input from the user.
9
