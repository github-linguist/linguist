socket = require "socket"
host, port = "127.0.0.1", 256

sid = socket.udp()
sid:sendto( "hello socket world", host, port )
sid:close()
