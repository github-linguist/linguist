name := "sample"
waitsec := 5
Tooltip Recording %name%.wav
MCI_SendString("close all wait")
MCI_SendString("open new type waveaudio alias " . name)
MCI_SendString("set " . name . " time format ms wait")
;MCI_SendString("set " . name . " bitspersample 16 wait")
;MCI_SendString("set " . name . " channels 1 wait")
;MCI_SendString("set " . name . " samplespersec 16000 wait")
;MCI_SendString("set " . name . " alignment 1 wait")
;MCI_SendString("set " . name . " bytespersec 8000 wait")
MCI_SendString("record " . name)
Sleep waitsec*1000
MCI_SendString("stop " . name . " wait")
MCI_SendString("save " . name . " """ . name . ".wav""")
Tooltip Finished ... Playing
MCI_SendString("delete " . name)
MCI_SendString("close " . name . " wait")
MCI_SendString("open """ . name . ".wav"" type waveaudio alias " . name)
MCI_SendString("play " . name . " wait")
MCI_SendString("close " . name . " wait")
Tooltip
Return

MCI_SendString(p_lpszCommand,ByRef r_lpszReturnString="",p_hwndCallback=0) {
	VarSetCapacity(r_lpszReturnString,512,0)
	Return DllCall("winmm.dll\mciSendString" . (A_IsUnicode ? "W":"A")
		,"Str",p_lpszCommand						;-- lpszCommand
		,"Str",r_lpszReturnString					;-- lpszReturnString
		,"UInt",512									;-- cchReturn
		,A_PtrSize ? "Ptr":"UInt",p_hwndCallback	;-- hwndCallback
		,"Cdecl Int")								;-- Return type
}
; For more intuitive functions, see the MCI library by jballi.
; doc: http://www.autohotkey.net/~jballi/MCI/v1.1/MCI.html
; download: http://www.autohotkey.net/~jballi/MCI/v1.1/MCI.ahk
