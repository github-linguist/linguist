SetBatchLines -1 ; used to improve performance
; (you can make it much faster by removing the informative tooltips)

;********************
; CONFIG
;********************
StartingNumber := 10
NumberLimit := 126030
CounterLimit := 25 ; calculations stop when one of these limits is reached
AdditionalNumbers := "16758243290880,24959017348650,14593825548650"
;********************

CurrentCounter := 0, CurrentNumber := StartingNumber

Loop  {
    if !Mod(A_Index,75) ; informative tooltip (every 75 calculations, to avoid slowing down)
        ToolTip, % "Checking numbers...`nNumber: " CurrentNumber
            . "/" NumberLimit "`nFound: " CurrentCounter "/" CounterLimit
    if ( CurrentCounter >= CounterLimit ) || ( CurrentNumber >= NumberLimit )
        Break
    if Mod(StrLen(CurrentNumber),2)
        CurrentNumber *= 10
    else if ( ( CurrentResult := GetFangs(CurrentNumber) ) <> "" )
        Output .= "`n" CurrentNumber ":`t" CurrentResult, CurrentCounter++
    CurrentNumber++
}
ToolTip ; hide informative tooltip

MsgBox % SubStr(Output,2) ; show output (first part)

Output := ""
Loop, Parse, AdditionalNumbers, % ","
{
    ToolTip, % "Getting fangs for " A_LoopField " ..." ; informative tooltip
    Output .= "`n" A_LoopField ":`n`t" GetFangs(A_LoopField) "`n"
}
ToolTip ; hide informative tooltip

MsgBox % SubStr(Output,2) ; show output (second part - additional numbers)
ExitApp

;----------------------------------------------------------------------------------

CharSorter(Input) { ; required by GetFangs()
    Loop, Parse, Input
        Output .= A_LoopField "`n"
    Sort, Output
    StringReplace, Output, Output, % "`n",, All
    Return Output
}

;----------------------------------------------------------------------------------

GetFangs(CurrentNumber) { ; requires CharSorter()
    ResultIndex := 1
    Length := StrLen(CurrentNumber)
    Power := (Length//2)-1
    if Mod(Length,2) OR !Power
        Return ""
    NumberLimit := Floor(Sqrt(CurrentNumber))
    Lower := 10 ** Power
    Loop, % NumberLimit - Lower {
        if !Mod(CurrentNumber,Lower) {
            FactorTwo := CurrentNumber//Lower
            if ( !Mod(Lower,10) && !Mod(FactorTwo,10) )
                Return ""
            Check := CharSorter( Lower . FactorTwo )
            if (CharSorter(CurrentNumber) = Check) && (StrLen(Lower) = StrLen(FactorTwo))
                Output .= "`n`t[" Lower "," FactorTwo "]"
        }
        Lower++
    }
    Return SubStr(Output,3) ; 3 = 1 + length of "`n`t"
}
