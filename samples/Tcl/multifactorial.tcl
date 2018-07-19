package require Tcl 8.6

proc mfact {n m} {
    set mm [expr {-$m}]
    for {set r $n} {[incr n $mm] > 1} {set r [expr {$r * $n}]} {}
    return $r
}

foreach n {1 2 3 4 5 6 7 8 9 10} {
    puts $n:[join [lmap m {1 2 3 4 5 6 7 8 9 10} {mfact $m $n}] ,]
}
