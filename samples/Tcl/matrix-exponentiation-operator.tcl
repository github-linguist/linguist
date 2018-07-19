package require Tcl 8.5
namespace path {::tcl::mathop ::tcl::mathfunc}

proc matrix_exp {m pow} {
    if { ! [string is int -strict $pow]} {
        error "non-integer exponents not implemented"
    }
    if {$pow < 0} {
        error "negative exponents not implemented"
    }
    lassign [size $m] rows cols
    # assume square matrix
    set temp [identity $rows]
    for {set n 1} {$n <= $pow} {incr n} {
        set temp [matrix_multiply $temp $m]
    }
    return $temp
}

proc identity {size} {
    set i [lrepeat $size [lrepeat $size 0]]
    for {set n 0} {$n < $size} {incr n} {lset i $n $n 1}
    return $i
}
