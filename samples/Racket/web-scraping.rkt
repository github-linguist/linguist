#lang racket
(require net/url)
((compose1 car (curry regexp-match #rx"[^ <>][^<>]+ UTC")
           port->string get-pure-port string->url)
 "http://tycho.usno.navy.mil/cgi-bin/timer.pl")
