package require math::complexnumbers
namespace import math::complexnumbers::*

set a [complex 1 1]
set b [complex 3.14159 1.2]
puts [tostring [+ $a $b]] ;# ==> 4.14159+2.2i
puts [tostring [* $a $b]] ;# ==> 1.94159+4.34159i
puts [tostring [pow $a [complex -1 0]]] ;# ==> 0.5-0.4999999999999999i
puts [tostring [- $a]] ;# ==> -1.0-i
