i = 10
Loop, % i {
  Random, v, -3.141592, 3.141592
  list .= v "`n"
  sum += v
}
MsgBox, % i ? list "`nmean: " sum/i:0
