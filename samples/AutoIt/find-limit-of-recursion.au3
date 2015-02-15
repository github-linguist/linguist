;AutoIt Version: 3.2.10.0
$depth=0
recurse($depth)
Func recurse($depth)
   ConsoleWrite($depth&@CRLF)
   Return recurse($depth+1)
EndFunc
