Loop, 10 {
  Delimiter := (A_Index = 5) || (A_Index = 10) ? "`n":", "
  Index .= A_Index . Delimiter
}
MsgBox %Index%
