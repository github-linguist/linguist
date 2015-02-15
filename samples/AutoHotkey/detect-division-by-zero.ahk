ZeroDiv(num1, num2) {
  If ((num1/num2) != "")
    MsgBox % num1/num2
  Else
    MsgBox, 48, Warning, The result is not valid (Divide By Zero).
}
ZeroDiv(0, 3) ; is ok
ZeroDiv(3, 0) ; divize by zero alert
