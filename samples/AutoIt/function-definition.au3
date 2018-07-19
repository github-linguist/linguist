#AutoIt Version: 3.2.10.0
$I=11
$J=12
MsgBox(0,"Multiply", $I &" * "& $J &" = " & product($I,$J))
Func product($a,$b)
   Return $a * $b
EndFunc
