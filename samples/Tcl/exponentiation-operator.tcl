package require Tcl 8.5
proc tcl::mathfunc::mypow {a b} {
    if { ! [string is int -strict $b]} {error "exponent must be an integer"}
    set res 1
    for {set i 1} {$i <= $b} {incr i} {set res [expr {$res * $a}]}
    return $res
}
expr {mypow(3, 3)} ;# ==> 27
expr {mypow(3.5, 3)} ;# ==> 42.875
expr {mypow(3.5, 3.2)} ;# ==> exponent must be an integer
