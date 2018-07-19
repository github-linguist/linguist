"c" ; character
"text" ; string
hereString =   ; with interpolation of %variables%
(
"<>"
the time is %A_Now%
\!
)

hereString2 =  ; with same line comments allowed, without interpolation of variables
(Comments %
literal %A_Now%  ; no interpolation here
)
