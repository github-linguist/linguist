package require Tcl 8.5

proc beadsort numList {
    # Special case: empty list is empty when sorted.
    if {![llength $numList]} return
    # Set up the abacus...
    foreach n $numList {
	for {set i 0} {$i<$n} {incr i} {
	    dict incr vals $i
	}
    }
    # Make the beads fall...
    foreach n [dict values $vals] {
	for {set i 0} {$i<$n} {incr i} {
	    dict incr result $i
	}
    }
    # And the result is...
    dict values $result
}

# Demonstration code
puts [beadsort {5 3 1 7 4 1 1}]
