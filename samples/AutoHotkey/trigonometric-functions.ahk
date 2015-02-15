pi := 4 * atan(1)
  radians := pi / 4
  degrees := 45.0
  result .= "`n" . sin(radians) . "     " . sin(degrees * pi / 180)
  result .= "`n" . cos(radians) . "     " . cos(degrees * pi / 180)
  result .= "`n" . tan(radians) . "     " . tan(degrees * pi / 180)

  temp := asin(sin(radians))
  result .= "`n" . temp . "     " . temp * 180 / pi

  temp := acos(cos(radians))
  result .= "`n" . temp . "     " . temp * 180 / pi

  temp := atan(tan(radians))
  result .= "`n" . temp . "     " . temp * 180 / pi

msgbox % result
/* output
---------------------------
trig.ahk
---------------------------
0.707107     0.707107
0.707107     0.707107
1.000000     1.000000
0.785398     45.000000
0.785398     45.000000
0.785398     45.000000
*/
