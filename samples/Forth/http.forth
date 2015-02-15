include unix/socket.fs

s" localhost" 80 open-socket
dup s\" GET / HTTP/1.0\n\n" rot write-socket
dup pad 8092 read-socket  type
close-socket
