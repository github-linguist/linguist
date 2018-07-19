package require Tcl 8.5
package require struct::list

proc gnomesort {a} {
    set i 1
    set j 2
    set size [llength $a]
    while {$i < $size} {
        if {[lindex $a [expr {$i - 1}]] <= [lindex $a $i]} {
            set i $j
            incr j
        } else {
            struct::list swap a [expr {$i - 1}] $i
            incr i -1
            if {$i == 0} {
                set i $j
                incr j
            }
        }
    }
    return $a
}

puts [gnomesort {8 6 4 2 1 3 5 7 9}] ;# => 1 2 3 4 5 6 7 8 9
