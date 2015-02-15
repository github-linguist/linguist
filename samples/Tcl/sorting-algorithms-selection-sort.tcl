package require Tcl 8.5
package require struct::list

proc selectionsort {A} {
    set len [llength $A]
    for {set i 0} {$i < $len - 1} {incr i} {
        set min_idx [expr {$i + 1}]
        for {set j $min_idx} {$j < $len} {incr j} {
            if {[lindex $A $j] < [lindex $A $min_idx]} {
                set min_idx $j
            }
        }
        if {[lindex $A $i] > [lindex $A $min_idx]} {
            struct::list swap A $i $min_idx
        }
    }
    return $A
}

puts [selectionsort {8 6 4 2 1 3 5 7 9}] ;# => 1 2 3 4 5 6 7 8 9
