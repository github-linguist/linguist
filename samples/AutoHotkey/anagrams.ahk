FileRead, Contents, unixdict.txt
Loop, Parse, Contents, % "`n", % "`r"
{ ; parsing each line of the file we just read
    Loop, Parse, A_LoopField ; parsing each letter/character of the current word
        Dummy .= "," A_LoopField
    Sort, Dummy, % "D," ; sorting those letters before removing the delimiters (comma)
    StringReplace, Dummy, Dummy, % ",", % "", All
    List .= "`n" Dummy " " A_LoopField , Dummy := ""
} ; at this point, we have a list where each line looks like <LETTERS><SPACE><WORD>
Count := 0, Contents := "", List := SubStr(List,2)
Sort, List
Loop, Parse, List, % "`n", % "`r"
{ ; now the list is sorted, parse it counting the consecutive lines with the same set of <LETTERS>
    Max := (Count > Max) ? Count : Max
    StringSplit, LinePart, A_LoopField, % " " ; (LinePart1 are the letters, LinePart2 is the word)
    If ( PreviousLinePart1 = LinePart1 )
        Count++ , WordList .= "," LinePart2
    Else
        var_Result .= ( Count <> Max ) ? "" ; don't append if the number of common words is too low
        : "`n" Count "`t" PreviousLinePart1 "`t" SubStr(WordList,2)
        , WordList := "" , Count := 0
    PreviousLinePart1 := LinePart1
}
List := "", var_Result := SubStr(var_Result,2)
Sort, var_Result, R N ; make the higher scores appear first
Loop, Parse, var_Result, % "`n", % "`r"
    If ( 1 == InStr(A_LoopField,Max) )
        var_Output .= "`n" A_LoopField
    Else ; output only those sets of letters that scored the maximum amount of common words
        Break
MsgBox, % ClipBoard := SubStr(var_Output,2) ; the result is also copied to the clipboard
