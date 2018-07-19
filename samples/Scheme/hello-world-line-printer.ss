(call-with-output-file "/dev/lp0"
  (lambda (printer)
    (write "Hello World!" printer)))
