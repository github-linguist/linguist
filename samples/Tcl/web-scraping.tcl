package require http

set request [http::geturl "http://tycho.usno.navy.mil/cgi-bin/timer.pl"]
if {[regexp -line {<BR>(.* UTC)} [http::data $request] --> utc]} {
    puts $utc
}
