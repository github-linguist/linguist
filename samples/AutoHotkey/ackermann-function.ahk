A(m, n) {
  If (m > 0) && (n = 0)
    Return A(m-1,1)
  Else If (m > 0) && (n > 0)
    Return A(m-1,A(m, n-1))
  Else If (m=0)
    Return n+1
}

; Example:
MsgBox, % "A(1,2) = " A(1,2)
