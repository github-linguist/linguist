Delims := "#;"
str := "apples, pears # and bananas"
str2:= "apples, pears, `; and bananas" ; needed to escape the ; since that is AHK's comment marker
msgbox % StripComments(Str,Delims)
msgbox % StripComments(Str2,Delims)
; The % forces expression mode.


StripComments(String1,Delims){
    Loop, parse, delims
    {
        If Instr(String1,A_LoopField)
            EndPosition := InStr(String1,A_LoopField) - 1
        Else
            EndPosition := StrLen(String1)
        StringLeft, String1, String1, EndPosition
    }
    return String1
}
