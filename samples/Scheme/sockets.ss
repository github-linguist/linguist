(let ((s (socket PF_INET SOCK_STREAM 0)))
    (connect s AF_INET (inet-pton AF_INET "127.0.0.1") 256)
    (display "hello socket world" s))
