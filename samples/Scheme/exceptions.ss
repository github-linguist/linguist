(define (me-errors xx exception)
  (if (even? xx)
      xx
      (exception)))

;example that does nothing special on exception
(call/cc
  (lambda (exception)
    (me-errors 222 exception)
    (display "I guess everything is alright")))

;example that laments oddness on exception
(call/cc
  (lambda (all-ok) ;used to "jump" over exception handling

    (call/cc
      (lambda (exception-handle)
        (me-errors 333 exception-handle)
        (display "I guess everything is alright")
        (all-ok)))

    (display "oh my god it is ODD!")))
