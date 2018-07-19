require 'socket'
sock = TCPSocket.open("localhost", 256)
sock.write("hello socket world")
sock.close
