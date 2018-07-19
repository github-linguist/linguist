(second (re-find #" (\d{1,2}:\d{1,2}:\d{1,2}) UTC" (slurp "http://tycho.usno.navy.mil/cgi-bin/timer.pl")))
