#include <Array.au3> ;Include extended Array functions (_ArrayDisplay)

Local $aInputs[1] ;Create the Array with just 1 element

While True ;Endless loop
	$aInputs[UBound($aInputs) - 1] = InputBox("Array", "Add one value") ;Save user input to the last element of the Array
	If $aInputs[UBound($aInputs) - 1] = "" Then ;If an empty string is entered, then...
		ReDim $aInputs[UBound($aInputs) - 1] ;...remove them from the Array and...
		ExitLoop ;... exit the loop!
	EndIf
	ReDim $aInputs[UBound($aInputs) + 1] ;Add an empty element to the Array
WEnd

_ArrayDisplay($aInputs) ;Display the Array
