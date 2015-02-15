;AutoIt Version: 3.2.10.0
$num = 45
MsgBox (0,"Factors", "Factors of " & $num & " are: " & factors($num))
consolewrite ("Factors of " & $num & " are: " & factors($num))
Func factors($intg)
   $ls_factors=""
   For $i = 1 to $intg/2
      if ($intg/$i - int($intg/$i))=0 Then
	 $ls_factors=$ls_factors&$i &", "
      EndIf
   Next
   Return $ls_factors&$intg
EndFunc
