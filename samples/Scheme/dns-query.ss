; Query DNS
(define n (car (hostent:addr-list (gethost "www.kame.net"))))

; Display address as IPv4 and IPv6
(display (inet-ntoa n))(newline)
(display (inet-ntop AF_INET n))(newline)
(display (inet-ntop AF_INET6 n))(newline)
