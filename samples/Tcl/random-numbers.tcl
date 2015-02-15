package require Tcl 8.5
variable ::pi [expr acos(0)]
proc ::tcl::mathfunc::nrand {} {
    expr {sqrt(-2*log(rand())) * cos(2*$::pi*rand())}
}

set mean 1.0
set stddev 0.5
for {set i 0} {$i < 1000} {incr i} {
    lappend result [expr {$mean + $stddev*nrand()}]
}
