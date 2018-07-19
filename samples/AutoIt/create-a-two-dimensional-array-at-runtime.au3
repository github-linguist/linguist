; == get dimensions from user input
$sInput = InputBox('2D Array Creation', 'Input comma separated count of rows and columns, i.e. "5,3"')
$aDimension = StringSplit($sInput, ',', 2)

; == create array
Dim $a2D[ $aDimension[0] ][ $aDimension[1] ]

; == write value to last row/last column
$a2D[ UBound($a2D) -1 ][ UBound($a2D, 2) -1 ] = 'test string'

; == output this value to MsgBox
MsgBox(0, 'Output', 'row[' & UBound($a2D) -1 & '], col[' & UBound($a2D, 2) -1 & ']' & @CRLF & '= ' & $a2D[ UBound($a2D) -1 ][ UBound($a2D, 2) -1 ] )
