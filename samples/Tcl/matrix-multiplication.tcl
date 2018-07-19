package require Tcl 8.5
namespace path ::tcl::mathop
proc matrix_multiply {a b} {
    lassign [size $a] a_rows a_cols
    lassign [size $b] b_rows b_cols
    if {$a_cols != $b_rows} {
        error "incompatible sizes: a($a_rows, $a_cols), b($b_rows, $b_cols)"
    }
    set temp [lrepeat $a_rows [lrepeat $b_cols 0]]
    for {set i 0} {$i < $a_rows} {incr i} {
        for {set j 0} {$j < $b_cols} {incr j} {
            set sum 0
            for {set k 0} {$k < $a_cols} {incr k} {
                set sum [+ $sum [* [lindex $a $i $k] [lindex $b $k $j]]]
            }
            lset temp $i $j $sum
        }
    }
    return $temp
}
