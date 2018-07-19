DateString := "March 7 2009 7:30pm EST"

; split the given string with RegExMatch
Needle := "^(?P<mm>\S*) (?P<d>\S*) (?P<y>\S*) (?P<t>\S*) (?P<tz>\S*)$"
RegExMatch(DateString, Needle, $)

; split the time with RegExMatch
Needle := "^(?P<h>\d+):(?P<min>\d+)(?P<xm>[amp]+)$"
RegExMatch($t, Needle, $)

; convert am/pm to 24h format
$h += ($xm = "am") ? 0 : 12

; knitting YYYYMMDDHH24MI format
_YYYY := $y
_MM   := Get_MonthNr($mm)
_DD   := SubStr("00" $d, -1) ; last 2 chars
_HH24 := SubStr("00" $h, -1) ; last 2 chars
_MI   := $min
YYYYMMDDHH24MI := _YYYY _MM _DD _HH24 _MI

; add 12 hours as requested
EnvAdd, YYYYMMDDHH24MI, 12, Hours
FormatTime, HumanReadable, %YYYYMMDDHH24MI%, d/MMM/yyyy  HH:mm

; add 5 hours to convert to different timezone (GMT)
EnvAdd, YYYYMMDDHH24MI, 5, Hours
FormatTime, HumanReadable_GMT, %YYYYMMDDHH24MI%, d/MMM/yyyy  HH:mm

; output
MsgBox, % "Given: " DateString "`n`n"
        . "12 hours later:`n"
        . "(" $tz "):`t" HumanReadable "h`n"
        . "(GMT):`t" HumanReadable_GMT "h`n"


;---------------------------------------------------------------------------
Get_MonthNr(Month) { ; convert named month to 2-digit number
;---------------------------------------------------------------------------
    If (Month = "January")
        Result := "01"
    Else If (Month = "February")
        Result := "02"
    Else If (Month = "March")
        Result := "03"
    Else If (Month = "April")
        Result := "04"
    Else If (Month = "May")
        Result := "05"
    Else If (Month = "June")
        Result := "06"
    Else If (Month = "July")
        Result := "07"
    Else If (Month = "August")
        Result := "08"
    Else If (Month = "September")
        Result := "09"
    Else If (Month = "October")
        Result := "10"
    Else If (Month = "November")
        Result := "11"
    Else If (Month = "December")
        Result := "12"
    Return, Result
}
