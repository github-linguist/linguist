include unix/socket.fs

s" localhost" 256 open-socket

dup s" hello socket world" rot write-socket

close-socket
