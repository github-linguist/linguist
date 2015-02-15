package require Tcl 8.5
namespace path {::tcl::mathop}
proc spiral size {
    set m [lrepeat $size [lrepeat $size .]]
    set x 0; set dx 0
    set y -1; set dy 1
    set i -1
    while {$i < $size ** 2 - 1} {
        if {$dy == 0} {
            incr x $dx
            if {0 <= $x && $x < $size && [lindex $m $x $y] eq "."} {
                lset m $x $y [incr i]
            } else {
                # back up and change direction
                incr x [- $dx]
                set dy [- $dx]
                set dx 0
            }
        } else {
            incr y $dy
            if {0 <= $y && $y < $size && [lindex $m $x $y] eq "."} {
                lset m $x $y [incr i]
            } else {
                # back up and  change direction
                incr y [- $dy]
                set dx $dy
                set dy 0
            }
        }
    }
    return $m
}

print_matrix [spiral 5]
