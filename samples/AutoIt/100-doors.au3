#include <array.au3>
$doors = 100

;door array, 0 = closed, 1 = open
Local $door[$doors +1]

For $ii = 1 To $doors
	For $i = $ii To $doors Step $ii
		$door[$i] = Not $door[$i]
	next
Next

;display to screen
For $i = 1 To $doors
	ConsoleWrite (Number($door[$i])& " ")
	If Mod($i,10) = 0 Then ConsoleWrite(@CRLF)
Next
