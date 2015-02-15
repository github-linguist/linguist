DllCall("AllocConsole")  ; Open a console window for this application
Pi := 4*ATan(1)
,Degrees := Pi/180

FileAppend, Enter Latitude: , CONOUT$ ; write to  stdout
FileReadLine, latitude, CONIN$, 1     ; read from stdin

FileAppend, Enter Longitude: , CONOUT$
FileReadLine, longitude, CONIN$, 1

FileAppend, Enter Legal meridian: , CONOUT$
FileReadLine, meridian, CONIN$, 1

sineLatitude := Sin(latitude*Degrees)
FileAppend, `n, CONOUT$
FileAppend, Sine of latitude: %sineLatitude%`n, CONOUT$
FileAppend, % "Difference of Longitudes (given longitude - meridian): " . longitude-meridian . "`n", CONOUT$
FileAppend, `n, CONOUT$

FileAppend, Numbers from 6 AM to 6 PM:`n, CONOUT$
FileAppend, Hour`t`tSun Hour Angle`t Dial hour line angle`n, CONOUT$


hour := -7
While (++hour < 7)
{
   clockHour := hour < 0 ? abs(hour) . "AM" : hour . "PM"
   shr := RTrim("" . (15.0*hour - (longitude-meridian)), "0") ; RTrim() removes trailing zeroes
   dhla := Atan(sineLatitude*Tan(shr*degrees))/Degrees
   FileAppend, %clockhour%`t`t%shr%`t`t%dhla%`n, CONOUT$
}
MsgBox close me when done.
