; Use the regular expression module to parse the url
(use-modules (ice-9 regex) (ice-9 rdelim))

; Variable to store result
(define time "")

; Set the url and parse the hostname, port, and path into variables
(define url "http://tycho.usno.navy.mil/cgi-bin/timer.pl")
(define r (make-regexp "^(http://)?([^:/]+)(:)?(([0-9])+)?(/.*)?" regexp/icase))
(define host (match:substring (regexp-exec r url) 2))
(define port (match:substring (regexp-exec r url) 4))
(define path (match:substring (regexp-exec r url) 6))

; Set port to 80 if it wasn't set above and convert from a string to a number
(if (eq? port #f) (define port "80"))
(define port (string->number port))

; Connect to remote host on specified port
(let ((s (socket PF_INET SOCK_STREAM 0)))
        (connect s AF_INET (car (hostent:addr-list (gethostbyname host))) port)

; Send a HTTP request for the specified path
        (display "GET " s)
        (display path s)
        (display " HTTP/1.0\r\n\r\n" s)

        (set! r (make-regexp "<BR>(.+? UTC)"))
        (do ((line (read-line s) (read-line s))) ((eof-object? line))
                (if (regexp-match? (regexp-exec r line))
                        (set! time (match:substring (regexp-exec r line) 1)))))

; Display result
(display time)
(newline)
