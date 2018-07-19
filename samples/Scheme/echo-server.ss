; Needed in Guile for read-line
(use-modules (ice-9 rdelim))

; Variable used to hold child PID returned from forking
(define child #f)

; Start listening on port 12321 for connections from any address
(let ((s (socket PF_INET SOCK_STREAM 0)))
  (setsockopt s SOL_SOCKET SO_REUSEADDR 1)
  (bind s AF_INET INADDR_ANY 12321)
  (listen s 5) ; Queue size of 5

  (simple-format #t "Listening for clients in pid: ~S" (getpid))
  (newline)

; Wait for connections forever
  (while #t
    (let* ((client-connection (accept s))
        (client-details (cdr client-connection))
        (client (car client-connection)))
; Once something connects fork
      (set! child (primitive-fork))
      (if (zero? child)
      (begin
; Then have child fork to avoid zombie children (grandchildren aren't our responsibility)
        (set! child (primitive-fork))
        (if (zero? child)
          (begin
; Display some connection details
          (simple-format #t "Got new client connection: ~S" client-details)
          (newline)
          (simple-format #t "Client address: ~S"
            (gethostbyaddr (sockaddr:addr client-details)))
          (newline)
; Wait for input from client and then echo the input back forever (or until client quits)
          (do ((line (read-line client)(read-line client))) ((zero? 1))
            (display line client)(newline client))))
; Child exits after spawning grandchild.
        (primitive-exit))
; Parent waits for child to finish spawning grandchild
      (waitpid child)))))
