package require Tcl 8.5
proc combrepl {set n {presorted no}} {
    if {!$presorted} {
        set set [lsort $set]
    }
    if {[incr n 0] < 1} {
	return {}
    } elseif {$n < 2} {
	return $set
    }
    # Recursive call
    set res [combrepl $set [incr n -1] yes]
    set result {}
    foreach item $set {
	foreach inner $res {
	    dict set result [lsort [list $item {*}$inner]] {}
	}
    }
    return [dict keys $result]
}

puts [combrepl {iced jam plain} 2]
puts [llength [combrepl {1 2 3 4 5 6 7 8 9 10} 3]]
