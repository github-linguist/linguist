URLDownloadToFile_( #Null, "http://tycho.usno.navy.mil/cgi-bin/timer.pl", "timer.htm", 0, #Null)
ReadFile(0, "timer.htm")
While Not Eof(0)    :    Text$ + ReadString(0)    :    Wend
MessageRequester("Time", Mid(Text$, FindString(Text$, "UTC", 1) - 9 , 8))
