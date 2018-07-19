include unix/socket.fs

128 constant size

: (echo) ( sock buf -- sock buf )
  begin
    cr ." waiting..."
    2dup 2dup size read-socket nip
    dup 0>
  while
    ."  got: " 2dup type
    rot write-socket
  repeat
  drop drop drop ;

create buf size allot

: echo-server ( port -- )
  cr ." Listening on " dup .
  create-server
  dup 4 listen
  begin
    dup accept-socket
    cr ." Connection!"
    buf ['] (echo) catch
    cr ." Disconnected (" . ." )"
    drop close-socket
  again ;

12321 echo-server
