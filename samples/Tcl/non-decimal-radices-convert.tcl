namespace eval baseconvert {
    variable chars "0123456789abcdefghijklmnopqrstuvwxyz"
    namespace export baseconvert
}
proc baseconvert::dec2base {n b} {
    variable chars
    expr {$n == 0 ? 0
          : "[string trimleft [dec2base [expr {$n/$b}] $b] 0][string index $chars [expr {$n%$b}]]"
    }
}
proc baseconvert::base2dec {n b} {
    variable chars
    set sum 0
    foreach char [split $n ""] {
        set d [string first $char [string range $chars 0 [expr {$b - 1}]]]
        if {$d == -1} {error "invalid base-$b digit '$char' in $n"}
        set sum [expr {$sum * $b + $d}]
    }
    return $sum
}
proc baseconvert::baseconvert {n basefrom baseto} {
    dec2base [base2dec $n $basefrom] $baseto
}

namespace import baseconvert::baseconvert
baseconvert 12345 10 23 ;# ==> 107h
baseconvert 107h 23 7   ;# ==> 50664
baseconvert 50664 7 10  ;# ==> 12345
