(do ((n 1024 (quotient n 2)))
    ((<= n 0))
    (display n)
    (newline))
