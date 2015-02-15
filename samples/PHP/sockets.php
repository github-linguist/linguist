$socket = fsockopen('localhost', 256);
fputs($socket, 'hello socket world');
fclose($socket);
