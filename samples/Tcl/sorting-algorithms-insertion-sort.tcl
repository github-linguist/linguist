package require Tcl 8.5

proc insertionsort {m} {
    for {set i 1} {$i < [llength $m]} {incr i} {
        set val [lindex $m $i]
        set j [expr {$i - 1}]
        while {$j >= 0 && [lindex $m $j] > $val} {
            lset m [expr {$j + 1}] [lindex $m $j]
            incr j -1
        }
        lset m [expr {$j + 1}] $val
    }
    return $m
}

puts [insertionsort {8 6 4 2 1 3 5 7 9}] ;# => 1 2 3 4 5 6 7 8 9
