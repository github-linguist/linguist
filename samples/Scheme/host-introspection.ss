(define host-info
  (begin
    (display "Endianness: ")
    (display (machine-byte-order))
    (newline)
    (display "Word Size: ")
    (display (if (fixnum? (expt 2 33)) 64 32))
    (newline)))
