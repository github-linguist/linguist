;AutoIt Version: 3.2.10.0
$A=4913
$n=3
$x=20
ConsoleWrite ($n& " root of "& $A & " is " &nth_root_it($A,$n,$x))
ConsoleWrite ($n& " root of "& $A & " is " &nth_root_rec($A,$n,$x))

;Iterative
Func nth_root_it($A,$n,$x)
   $x0="0"
   While StringCompare(string($x0),string($x))
      ConsoleWrite ($x&@CRLF)
      $x0=$x
      $x=((($n-1)*$x)+($A/$x^($n-1)))/$n
   WEnd
   Return $x
EndFunc

;Recursive
Func nth_root_rec($A,$n,$x)
   ConsoleWrite ($x&@CRLF)
   If $x==((($n-1)*$x)+($A/$x^($n-1)))/$n Then
      Return $x
   EndIf
   Return nth_root_rec($A,$n,((($n-1)*$x)+($A/$x^($n-1)))/$n)
EndFunc
