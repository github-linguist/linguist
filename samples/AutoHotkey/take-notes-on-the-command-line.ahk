Notes := "Notes.txt"

If 0 = 0 ; no arguments
{
    If FileExist(Notes) {
        FileRead, Content, %Notes%
        MsgBox, %Content%
    } Else
        MsgBox, %Notes% does not exist
    Goto, EOF
}

; date and time, colon, newline (CRLF), tab
Date := A_DD "/" A_MM "/" A_YYYY
Time := A_Hour ":" A_Min ":" A_Sec "." A_MSec
FileAppend, %Date% %Time%:`r`n%A_Tab%, %Notes%

; command line parameters, trailing newline (CRLF)
Loop, %0%
    FileAppend, % %A_Index% " ", %Notes%
FileAppend, `r`n, %Notes%

EOF:
