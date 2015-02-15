var http = require('http');

http.createServer(function (req, res) {
    res.writeHead(200, {'Content-Type': 'text/plain'});
    res.end('Goodbye, World!\n');
}).listen(8080, '127.0.0.1');
