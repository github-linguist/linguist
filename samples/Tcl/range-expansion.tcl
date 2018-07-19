proc rangeExpand desc {
    set result {}
    foreach term [split $desc ","] {
	set count [scan $term %d-%d from to]
	if {$count == 1} {
	    lappend result $from
	} elseif {$count == 2} {
	    for {set i $from} {$i <= $to} {incr i} {lappend result $i}
	}
    }
    return $result
}

puts [rangeExpand "-6,-3--1,3-5,7-11,14,15,17-20"]
