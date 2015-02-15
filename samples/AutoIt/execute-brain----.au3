; AutoFucck
; A AutoIt Brainfuck Interpreter
; by minx
; AutoIt Version: 3.3.8.x

; Commands:
; - 	DEC
; +		INC
; [		LOOP START
; ]		LOOP END
; .		Output cell value as ASCII Chr
; ,		Input a ASCII char (cell value = ASCII code)
; :		Ouput cell value as integer
; ;		Input a Integer
; _		Output a single whitespace
; / 	Output an Carriage Return and Line Feed

; You can load & save .atf Files.

#include <WindowsConstants.au3>
#include <EditConstants.au3>
#include <Array.au3>
#include <GUIConstants.au3>
#include <StaticCOnstants.au3>

HotKeySet("{F5}", "_Runn")

$hMain = GUICreate("Autofuck - Real Brainfuck Interpreter", 600, 525)
$mMain = GUICtrlCreateMenu("File")
Global $mCode = GUICtrlCreateMenu("Code")
$mInfo = GUICtrlCreateMenu("Info")
$mCredits = GUICtrlCreateMenuItem("Credits", $mInfo)
$mFile_New = GUICtrlCreateMenuItem("New", $mMain)
$mFile_Open = GUICtrlCreateMenuItem("Open", $mMain)
$mFile_Save = GUICtrlCreateMenuItem("Save", $mMain)
Global $mCode_Run = GUICtrlCreateMenuItem("Run [F5]", $mCode)
Global $lStatus = GUICtrlCreateLabel("++ Autofuck started...", 5, 480, 590, 20, $SS_SUNKEN)
GUICtrlSetFont(-1, Default, Default, Default, "Courier New")
$eCode = GUICtrlCreateEdit("", 5, 5, 590, 350)
GUICtrlSetFont(-1, Default, Default, Default, "Courier New")
$eConsole = GUICtrlCreateEdit("", 5, 360, 590, 115, $ES_WANTRETURN)
GUICtrlSetFont(-1, Default, Default, Default, "Courier New")
GUISetState()

While 1
	$nMsg = GUIGetMsg()
	Switch $nMsg
		Case $mFile_New
			GUICtrlSetData($eCode, "")
		Case $mFile_Open
			GUICtrlSetData($eCode, FileRead(FileOpenDialog("Open Autofuck script", @DesktopDir, "Autofuck (*.atf)")))
		Case $mFile_Save
			FileWrite(FileOpen(StringReplace(FileSaveDialog("Save Autofuck script", @DesktopDir, "Autofuck (*.atf)"), ".atf", "") &".atf", 2), GUICtrlRead($eCode))
		Case $GUI_EVENT_CLOSE
			Exit
		Case $mCredits
			MsgBox(0, "Autofuck", "Copyright by: "&@CRLF&"minx (autoit.de)"&@CRLF&"crashdemons (autoitscript.com)")
	EndSwitch
WEnd

Func _Runn()
	$Timer = TimerInit()
	GUICtrlSetData($lStatus, "++ Program started")
	Global $tData=DllStructCreate('BYTE[65536]')
	Global $pData=0
	GUICtrlSetData($eConsole, "")
	Local $aError[6]=['','Unmatched closing bracket during search','Unmatched opening bracket during search','Unexpected closing bracket','Data pointer passed left boundary','Data pointer passed right boundary']
    Local $sError=''
    Local $i=_Run(GUICtrlRead($eCode))
    If @error>=0 And @error<6 Then $sError=$aError[@error]
    If StringLen($sError) Then GUICtrlSetData($eConsole, 'ERROR: '&$sError&'.'&@CRLF&'Ending Instruction Pointer: '&($i-1)&@CRLF&'Current Data Pointer: '&$pData)
	GUICtrlSetData($lStatus, "++ Program terminated. Runtime: "& Round( TimerDiff($Timer) / 1000, 4) &"s")
EndFunc

Func _Run($Code,$iStart=1,$iEnd=0)
    If $iEnd<1 Then $iEnd=StringLen($Code)
    For $i = $iStart to $iEnd
        Switch StringMid($Code, $i, 1)
            Case ">"
                $pData+=1
                If $pData=65536 Then Return SetError(5,0,$i)
            Case "<"
                $pData-=1
                If $pData<0 Then Return SetError(4,0,$i)
            Case "+"
                DllStructSetData($tData,1,DllStructGetData($tData,1,$pData+1)+1,$pData+1)
            Case "-"
                DllStructSetData($tData,1,DllStructGetData($tData,1,$pData+1)-1,$pData+1)
            Case ":"
                GUICtrlSetData($eConsole, GUICtrlRead($eConsole) & (DllStructGetData($tData,1,$pData+1)))
			Case "."
                GUICtrlSetData($eConsole, GUICtrlRead($eConsole) & Chr(DllStructGetData($tData,1,$pData+1)))
            Case ";"
                Local $cIn=StringMid(InputBox('Autofuck','Enter Number'),1)
                DllStructSetData($tData,1,Number($cIn),$pData+1)
			Case ","
                Local $cIn=StringMid(InputBox('Autofuck','Enter one ASCII character'),1,1)
                DllStructSetData($tData,1,Asc($cIn),$pData+1)
            Case "["
                Local $iStartSub=$i
                Local $iEndSub=_MatchBracket($Code,$i,$iEnd)
                If @error<>0 Then Return SetError(@error,0,$iEndSub)
                While DllStructGetData($tData,1,$pData+1)<>0
                    Local $iRet=_Run($Code,$iStartSub+1,$iEndSub-1)
                    If @error<>0 Then Return SetError(@error,0,$iRet)
                WEnd
                $i=$iEndSub
            Case ']'
                Return SetError(3,0,$i)
			Case "_"
				GUICtrlSetData($eConsole, GUICtrlRead($eConsole)&" ")
			Case "/"
				GUICtrlSetData($eConsole, GUICtrlRead($eConsole)&@CRLF)
        EndSwitch
    Next
    Return 0
EndFunc

Func _MatchBracket($Code,$iStart=1,$iEnd=0)
    If $iEnd<1 Then $iEnd=StringLen($Code)
    Local $Open=0
    For $i=$iStart To $iEnd
        Switch StringMid($Code,$i,1)
            Case '['
                $Open+=1
            Case ']'
                $Open-=1
                If $Open=0 Then Return $i
                If $Open<0 Then Return SetError(1,0,$i)
        EndSwitch
    Next
    If $Open>0 Then Return SetError(2,0,$i)
    Return 0
EndFunc
