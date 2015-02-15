var net = require('net');

var server = net.createServer(function(conn) {
  console.log("Connection from " + conn.remoteAddress + " on port " + conn.remotePort);
  conn.setEncoding("utf8");
  var buffer = "";

  conn.on("data", function(data) {
    for(var i = 0; i <= data.length; i++) {
      var char = data.charAt(i);
      buffer += char;
      if(char == "\n") {
        conn.write(buffer);
        buffer = "";
      }
    }
  });
});

server.listen(12321, "localhost");
