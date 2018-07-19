package require Tcl 8.5

proc shellsort {m} {
    set len [llength $m]
    set inc [expr {$len / 2}]
    while {$inc > 0} {
        for {set i $inc} {$i < $len} {incr i} {
            set j $i
            set temp [lindex $m $i]
            while {$j >= $inc && [set val [lindex $m [expr {$j - $inc}]]] > $temp} {
                lset m $j $val
                incr j -$inc
            }
            lset m $j $temp
        }
        set inc [expr {$inc == 2 ? 1 : $inc * 5 / 11}]
    }
    return $m
}

puts [shellsort {8 6 4 2 1 3 5 7 9}] ;# => 1 2 3 4 5 6 7 8 9
