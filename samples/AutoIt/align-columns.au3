; == If the given text is in an file, it will read with:
#include <File.au3>
Global $aRead
_FileReadToArray($sPath, $aRead)  ; == $aRead[0] includes count of lines, every line stored in one item (without linebreak)

; == For example we get the same result with StringSplit()
Global $sText = _
"Given$a$text$file$of$many$lines,$where$fields$within$a$line$" & @CRLF & _
"are$delineated$by$a$single$'dollar'$character,$write$a$program" & @CRLF & _
"that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$" & @CRLF & _
"column$are$separated$by$at$least$one$space." & @CRLF & _
"Further,$allow$for$each$word$in$a$column$to$be$either$left$" & @CRLF & _
"justified,$right$justified,$or$center$justified$within$its$column." & @CRLF

$aRead = StringSplit($sText, @CRLF, 1)

; == strip leading and trailing "$" and trailing spaces, count remaining "$" to get max column number
Global $iMaxColumn = 0, $iLines = 0
For $i = 1 To $aRead[0]
	If $aRead[$i] = '' Then ContinueLoop   ; skip empty lines
	$iLines += 1
	$aRead[$i] = StringRegExpReplace(StringRegExpReplace(StringRegExpReplace($aRead[$i], '^\$', ''), '\$$', ''), '\s*$', '')
	StringReplace($aRead[$i], '$', '$')
	If @extended +1 > $iMaxColumn Then $iMaxColumn = @extended +1
Next

; == build array to store all fields and length of every item
Global $aFields[$iLines][$iMaxColumn +1][2]
; == and store the max. length of item in columns
Global $aColLen[$iMaxColumn]

; == fill the array
Global $aSplitLine
$iLines = 0
For $i = 1 To $aRead[0]
	If $aRead[$i] = '' Then ContinueLoop   ; skip empty lines
	$iMaxColLen = 0
	$aSplitLine = StringSplit($aRead[$i], '$')
	For $j = 1 To $aSplitLine[0]
		$aFields[$iLines][$j-1][0] = $aSplitLine[$j]
		$aFields[$iLines][$j-1][1] = StringLen($aSplitLine[$j])
		If $aFields[$iLines][$j-1][1] > $aColLen[$j-1] Then $aColLen[$j-1] = $aFields[$iLines][$j-1][1]
	Next
	$iLines += 1
Next

; == let the user select the alignment for every column
$sAlign = InputBox('Column alignment', 'There are ' & $iMaxColumn & ' columns.' & @LF & '0 = left    1 = center    2 = right' & @LF &  _
                   'Input alignment for all columns without delimiters.' & @LF & 'Let it empty, to align all left.')
If $sAlign = '' Then
	For $i = 1 To $iMaxColumn
		$sAlign &= '0'
	Next
EndIf
Global $aAlignment = StringSplit($sAlign, '', 2)

; == output all to console
Global $sLineOut
For $i = 0 To UBound($aFields) -1
	$sLineOut = ''
	For $j = 0 To $iMaxColumn -1
		If $aFields[$i][$j][0] = '' Then ContinueLoop
		$sLineOut &= _GetAligned($aFields[$i][$j][0], $aFields[$i][$j][1], $aAlignment[$j], $aColLen[$j])
	Next
	ConsoleWrite(StringTrimRight($sLineOut, 1) & @LF)
Next

Func _GetAligned($_sString, $_iLen, $_iAlign, $_iMaxLen)
	Local $sSpace = ''
	For $i = 1 To $_iMaxLen
		$sSpace &= ' '
	Next
	Switch $_iAlign
		Case 0
			Return $_sString & StringLeft($sSpace, $_iMaxLen - $_iLen +1)
		Case 1
			Local $iLenLeft = Int(($_iMaxLen - $_iLen)/2)
			Local $iLenRight = $_iMaxLen - $iLenLeft - $_iLen
			Return StringLeft($sSpace, $iLenLeft) & $_sString & StringLeft($sSpace, $iLenRight) & ' '
		Case 2
			Return StringLeft($sSpace, $_iMaxLen - $_iLen) & $_sString & ' '
	EndSwitch
EndFunc  ;==>_GetAligned
