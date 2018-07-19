test = StringSplit[Import["http://tycho.usno.navy.mil/cgi-bin/timer.pl"], "\n"];
Extract[test, Flatten@Position[StringFreeQ[test, "UTC"], False]]
