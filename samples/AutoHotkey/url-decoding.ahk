encURL := "http%3A%2F%2Ffoo%20bar%2F"
SetFormat, Integer, hex
Loop Parse, encURL
   If A_LoopField = `%
      reading := 2, read := ""
   else if reading
   {
      read .= A_LoopField, --reading
      if not reading
         out .= Chr("0x" . read)
   }
   else out .= A_LoopField
MsgBox % out ; http://foo bar/
