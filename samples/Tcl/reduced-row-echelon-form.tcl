package require Tcl 8.5
namespace path {::tcl::mathop ::tcl::mathfunc}

proc toRREF {m} {
    set lead 0
    lassign [size $m] rows cols
    for {set r 0} {$r < $rows} {incr r} {
        if {$cols <= $lead} {
            break
        }
        set i $r
        while {[lindex $m $i $lead] == 0} {
            incr i
            if {$rows == $i} {
                set i $r
                incr lead
                if {$cols == $lead} {
                    # Tcl can't break out of nested loops
                    return $m
                }
            }
        }
        # swap rows i and r
        foreach idx [list $i $r] row [list [lindex $m $r] [lindex $m $i]] {
            lset m $idx $row
        }
        # divide row r by m(r,lead)
        set val [lindex $m $r $lead]
        for {set j 0} {$j < $cols} {incr j} {
            lset m $r $j [/ [double [lindex $m $r $j]] $val]
        }

        for {set i 0} {$i < $rows} {incr i} {
            if {$i != $r} {
                # subtract m(i,lead) multiplied by row r from row i
                set val [lindex $m $i $lead]
                for {set j 0} {$j < $cols} {incr j} {
                    lset m $i $j [- [lindex $m $i $j] [* $val [lindex $m $r $j]]]
                }
            }
        }
        incr lead
    }
    return $m
}

set m {{1 2 -1 -4} {2 3 -1 -11} {-2 0 -3 22}}
print_matrix $m
print_matrix [toRREF $m]
