socket = connect("localhost",256)
write(socket, "hello socket world")
close(socket)
