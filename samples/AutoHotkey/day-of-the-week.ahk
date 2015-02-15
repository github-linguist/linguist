year = 2008
stop = 2121

While year <= stop {
 FormatTime, day,% year 1225, dddd
 If day = Sunday
  out .= year "`n"
 year++
}
MsgBox,% out
