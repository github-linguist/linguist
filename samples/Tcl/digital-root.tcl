package require Tcl 8.5
proc digitalroot num {
    for {set p 0} {[string length $num] > 1} {incr p} {
	set num [::tcl::mathop::+ {*}[split $num ""]]
    }
    list $p $num
}

foreach n {627615 39390 588225 393900588225} {
    lassign [digitalroot $n] p r
    puts [format "$n has additive persistence $p and digital root of $r"]
}
