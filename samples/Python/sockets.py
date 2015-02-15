import socket
sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
sock.connect(("localhost", 256))
sock.sendall("hello socket world")
sock.close()
