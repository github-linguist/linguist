MaxLen=0
Loop, Read, UnixDict.txt         ; Assigns A_LoopReadLine to each line of the file
{
    thisword := A_LoopReadLine   ; Just for readability
    blSort := isSorted(thisWord) ; reduce calls to IsSorted to improve performance
    ThisLen := StrLen(ThisWord)  ; reduce calls to StrLen to improve performance
    If (blSort = true and ThisLen = maxlen)
        list .= ", " . thisword
    Else If (blSort = true and ThisLen > maxlen)
    {
        list := thisword
        maxlen := ThisLen
    }
}

IsSorted(word){  ; This function uses the ASCII value of the letter to determine its place in the alphabet.
                           ;        Thankfully, the dictionary is in all lowercase
    lastchar=0
    Loop, parse, word
    {
        if ( Asc(A_LoopField) < lastchar )
            return false
        lastchar := Asc(A_loopField)
    }
    return true
}

GUI, Add, Edit, w300 ReadOnly, %list%
GUI, Show
return ; End Auto-Execute Section

GUIClose:
ExitApp
