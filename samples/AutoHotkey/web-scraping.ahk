UrlDownloadToFile, http://tycho.usno.navy.mil/cgi-bin/timer.pl, time.html
FileRead, timefile, time.html
pos := InStr(timefile, "UTC")
msgbox % time := SubStr(timefile, pos - 9, 8)
