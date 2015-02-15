package require Tcl 8.5
proc mean args {
    if {[set num [llength $args]] == 0} {return 0}
    expr {[tcl::mathop::+ {*}$args] / double($num)}
}
mean 3 1 4 1 5 9 ;# ==> 3.8333333333333335
