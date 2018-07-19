package require Tcl 8.5

proc unsafe {y} {
    global b
    set x [lindex $b $y]
    for {set i 1} {$i <= $y} {incr i} {
	set t [lindex $b [expr {$y - $i}]]
	if {$t==$x || $t==$x-$i || $t==$x+$i} {
	    return 1
	}
    }
    return 0
}

proc putboard {} {
    global b s N
    puts "\n\nSolution #[incr s]"
    for {set y 0} {$y < $N} {incr y} {
	for {set x 0} {$x < $N} {incr x} {
	    puts -nonewline [expr {[lindex $b $y] == $x ? "|Q" : "|_"}]
	}
	puts "|"
    }
}

proc main {n} {
    global b N
    set N $n
    set b [lrepeat $N 0]
    set y 0
    lset b 0 -1
    while {$y >= 0} {
	lset b $y [expr {[lindex $b $y] + 1}]
	while {[lindex $b $y] < $N && [unsafe $y]} {
	    lset b $y [expr {[lindex $b $y] + 1}]
	}
	if {[lindex $b $y] >= $N} {
	    incr y -1
	} elseif {$y < $N-1} {
	    lset b [incr y] -1;
	} else {
	    putboard
	}
    }
}

main [expr {$argc ? int(0+[lindex $argv 0]) : 8}]
