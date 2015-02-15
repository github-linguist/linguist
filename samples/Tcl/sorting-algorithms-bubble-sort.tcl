package require Tcl 8.5
package require struct::list

proc bubblesort {A} {
    set len [llength $A]
    set swapped true
    while {$swapped} {
        set swapped false
        for {set i 0} {$i < $len - 1} {incr i} {
            set j [expr {$i + 1}]
            if {[lindex $A $i] > [lindex $A $j]} {
                struct::list swap A $i $j
                set swapped true
            }
        }
        incr len -1
    }
    return $A
}

puts [bubblesort {8 6 4 2 1 3 5 7 9}] ;# => 1 2 3 4 5 6 7 8 9
