;AutoIt Version: 3.2.10.0
$num = "45  54"
consolewrite ("Sum of " & $num & " is: " & sum($num))
Func sum($numbers)
   $numm = StringSplit($numbers," ")
   Return $numm[1]+$numm[$numm[0]]
EndFunc
