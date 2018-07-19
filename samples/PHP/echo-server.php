$socket = socket_create(AF_INET,SOCK_STREAM,SOL_TCP);
socket_bind($socket, '127.0.0.1', 12321);
socket_listen($socket);

$client_count = 0;
while (true){
  if (($client = socket_accept($socket)) === false) continue;
  $client_count++;

  $client_name = 'Unknown';
  socket_getpeername($client, $client_name);
  echo "Client {$client_count} ({$client_name}) connected\n";
  $pid = pcntl_fork();
  if($pid == -1) die('Could not fork');
  if($pid){
    pcntl_waitpid(-1, $status, WNOHANG);
    continue;
  }

  //In a child process
  while(true){
    if($input = socket_read($client, 1024)){
      socket_write($client, $input);
    } else {
      socket_shutdown($client);
      socket_close($client);
      echo "Client {$client_count} ({$client_name}) disconnected\n";
      exit();
    }
  }
}
