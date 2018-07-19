include unix/socket.fs

: extract-time ( addr len type len -- time len )
  dup >r
  search 0= abort" that time not present!"
  dup >r
  begin -1 /string  over 1- c@ [char] > = until       \ seek back to <BR> at start of line
  r> - r> + ;

s" tycho.usno.navy.mil" 80 open-socket
dup s\" GET /cgi-bin/timer.pl HTTP/1.0\n\n" rot write-socket
dup pad 4096 read-socket
s\" \r\n\r\n" search 0= abort" can't find headers!"   \ skip headers
s" UTC" extract-time type cr
close-socket
