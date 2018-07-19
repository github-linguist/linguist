package require Tcl 8.6

oo::class create SofN {
    variable items size count
    constructor {n} {
	set size $n
    }
    method item {item} {
	if {[incr count] <= $size} {
	    lappend items $item
	} elseif {rand()*$count < $size} {
	    lset items [expr {int($size * rand())}] $item
	}
	return $items
    }
}

# Test code
for {set i 0} {$i < 100000} {incr i} {
    set sOf3 [SofN new 3]
    foreach digit {0 1 2 3 4 5 6 7 8 9} {
	set digs [$sOf3 item $digit]
    }
    $sOf3 destroy
    foreach digit $digs {
	incr freq($digit)
    }
}
parray freq
