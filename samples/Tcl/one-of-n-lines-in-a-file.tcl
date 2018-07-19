package require Tcl 8.5
proc 1ofN {n} {
    for {set line 1} {$line <= $n} {incr line} {
	if {rand() < 1.0/[incr fraction]} {
	    set result $line
	}
    }
    return $result
}

for {set i 0} {$i < 1000000} {incr i} {
    incr count([1ofN 10])
}
parray count;   # Alphabetic order, but convenient
