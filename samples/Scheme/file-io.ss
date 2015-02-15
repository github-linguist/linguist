; Open ports for the input and output files
(define in-file (open-input-file "input.txt"))
(define out-file (open-output-file "output.txt"))

; Read and write characters from the input file
; to the output file one by one until end of file
(do ((c (read-char in-file) (read-char in-file)))
        ((eof-object? c))
        (write-char c out-file))

; Close the ports
(close-input-port in-file)
(close-output-port out-file)
