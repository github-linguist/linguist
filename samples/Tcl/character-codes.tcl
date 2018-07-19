# ASCII
puts [scan "a" %c]   ;# ==> 97
puts [format %c 97]  ;# ==> a
# Unicode is the same
puts [scan "π" %c]   ;# ==> 960
puts [format %c 960] ;# ==> π
