net = require("net")
server = net.createServer (conn) ->
  console.log "Connection from #{conn.remoteAddress} on port  #{conn.remotePort}"
  conn.setEncoding "utf8"
  buffer = ""
  conn.on "data", (data) ->
    i = 0

    while i <= data.length
      char = data.charAt(i)
      buffer += char
      if char is "\n"
        conn.write buffer
        buffer = ""
      i++

server.listen 12321, "localhost"
