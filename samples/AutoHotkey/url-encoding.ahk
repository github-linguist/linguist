rawURL = http://foo bar/
SetFormat, Integer, Hex
Loop Parse, rawURL
   If A_LoopField is not alnum ; not a-zA-Z0-9
        encURL .= "%" . SubStr(Asc(A_LoopField), 3)
   else encURL .= A_LoopField
MsgBox % encURL
