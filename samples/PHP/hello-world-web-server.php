<?php
                     // AF_INET6 for IPv6  // IP
$socket = socket_create(AF_INET, SOCK_STREAM, 0) or die('Failed to create socket!');
                  // '127.0.0.1' to limit only to localhost // Port
socket_bind($socket, 0,                                        8080);
socket_listen($socket);

$msg = '<html><head><title>Goodbye, world!</title></head><body>Goodbye, world!</body></html>';

for (;;) {
    // @ is used to stop PHP from spamming with error messages if there is no connection
    if ($client = @socket_accept($socket)) {
        socket_write($client, "HTTP/1.1 200 OK\r\n" .
               "Content-length: " . strlen($msg) . "\r\n" .
               "Content-Type: text/html; charset=UTF-8\r\n\r\n" .
               $msg);
    }
    else usleep(100000); // limits CPU usage by sleeping after doing every request
}
?>
