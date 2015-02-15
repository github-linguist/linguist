package require Tcl 8.5

set values {4 3 2 7 8 9}
::tcl::mathfunc::max {*}$values ;# ==> 9
