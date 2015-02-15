USING: http.client io sequences ;

"http://tycho.usno.navy.mil/cgi-bin/timer.pl" http-get nip
[ "UTC" swap start [ 9 - ] [ 1 - ] bi ] keep subseq print
