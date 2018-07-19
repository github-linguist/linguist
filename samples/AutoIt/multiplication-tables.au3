#AutoIt Version: 3.2.10.0
$tableupto=12
$table=""
for $i = 1 To $tableupto
   for $j = $i to $tableupto
      $prod=string($i*$j)
      if StringLen($prod) == 1  then
	 $prod = "    "& $prod
      EndIf
      if StringLen($prod) == 2  then
	 $prod = "  "& $prod
      EndIf
      $table = $table&" "&$prod
   Next
   $table = $table&"  - "&$i&@CRLF
   for  $k = 1 to $i
      $table = $table&"       "
   Next
Next
msgbox(0,"Multiplication Tables",$table)
