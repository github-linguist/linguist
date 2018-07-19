SetWorkingDir %A_ScriptDir%
#NoEnv
Data := "12  127 28  42  39  113 42  18  44  118 44  37  113 124 37  48  127 36  29  31  125 139 131 115 105 132 104 123 35  113 122 42  117 119 58  109 23  105 63  27  44  105 99  41  128 121 116 125 32  61  37  127 29  113 121 58  114 126 53  114 96  25  109 7   31  141 46  13  27  43  117 116 27  7   68  40  31  115 124 42  128 52  71  118 117 38  27  106 33  117 116 111 40  119 47  105 57  122 109 124 115 43  120 43  27  27  18  28  48  125 107 114 34  133 45  120 30  127 31  116  146"
 ; This loop removes the double/multiple spaces encountered when copying+pasting the given data set:
While (Instr(Data,"  "))
    StringReplace, Data, Data,%A_Space%%A_Space%,%A_Space%,All
; Sort the data numerically using a space as the separator:
Sort, Data,ND%A_Space%

OldStem := 0
; Parse the data using a space as the separator, storing each new string as A_LoopField and running the loop once per string:
Loop, parse, Data,%A_Space%
{
    NewStem := SubStr(A_LoopField,1,StrLen(A_LoopField)-1)     ; AutoHotkey doesn't have a Left() function, so this does the trick.
    If ( NewStem <> OldStem  and StrLen(A_LoopField) <> 1)
    {
        While(OldStem+1<>NewStem)                              ; account for all stems which don't appear (in this example, 8) but are between the lowest and highest stems
            OldStem++,ToPrint .= "`n" PadStem(oldStem)
        ToPrint .= "`n" PadStem(NewStem)
        OldStem := NewStem
    }
    Else If ( StrLen(A_LoopField)=1 and !FirstStem)
        ToPrint .= PadStem(0),FirstStem := true
    ToPrint .= SubStr(A_LoopField,strLen(A_LoopField)) " "    ; No Right() function either, so this returns the last character of A_LoopField (the string curently used by the parsing loop)
}
                                                              ; Delete the old stem and leaf file (if any), write our new contents to it, then show it:
FileDelete Stem and leaf.txt
FileAppend %ToPrint%, Stem and Leaf.txt
Run Stem and leaf.txt
return

PadStem(Stem){
    Spaces = 0
    While ( 3 - StrLen(Stem) <> Spaces )                     ; If the stems are more than 2 digits long, increase the number 3 to one more than the stem length.
        ToReturn .= " ",Spaces++
    ToReturn .= Stem
    ToReturn .= " | "
    Return ToReturn
}
