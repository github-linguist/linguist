package require Tcl 8.5
namespace eval subrand {
    variable mod 1000000000 state [lrepeat 55 0] si 0 sj 0

    proc seed p1 {
	global subrand::mod subrand::state subrand::si subrand::sj
	set p2 1
	lset state 0 [expr {$p1 % $mod}]
	for {set i 1; set j 21} {$i < 55} {incr i; incr j 21} {
	    if {$j >= 55} {incr j -55}
	    lset state $j $p2
	    if {[set p2 [expr {$p1 - $p2}]] < 0} {incr p2 $mod}
	    set p1 [lindex $state $j]
	}
	set si 0
	set sj 24
	for {set i 0} {$i < 165} {incr i} { gen }
    }

    proc gen {} {
	global subrand::mod subrand::state subrand::si subrand::sj
	if {$si == $sj} {seed 0}
	if {[incr si -1] < 0} {set si 54}
	if {[incr sj -1] < 0} {set sj 54}
	set x [expr {[lindex $state $si] - [lindex $state $sj]}]
	if {$x < 0} {incr x $mod}
	lset state $si $x
	return $x
    }
}

subrand::seed 292929
for {set i 0} {$i < 10} {incr i} {
    puts [subrand::gen]
}
