proc merge {listVar toMerge} {
    upvar 1 $listVar v
    set i [set j 0]
    set out {}
    while {$i<[llength $v] && $j<[llength $toMerge]} {
	if {[set a [lindex $v $i]] < [set b [lindex $toMerge $j]]} {
	    lappend out $a
	    incr i
	} else {
	    lappend out $b
	    incr j
	}
    }
    # Done the merge, but will be one source with something left
    # This will handle all that by doing a merge of the remnants onto the end
    set v [concat $out [lrange $v $i end] [lrange $toMerge $j end]]
    return
}

proc strandSort A {
    set results {}
    while {[llength $A]} {
	set sublist [lrange $A 0 0]
	# We build a list of items that weren't filtered rather than removing "in place"
	# because this fits better with the way Tcl values work (the underlying data
	# structure is an array, not a linked list).
	set newA {}
	foreach a [lrange $A 1 end] {
	    if {$a > [lindex $sublist end]} {
		lappend sublist $a
	    } else {
		lappend newA $a
	    }
	}
	set A $newA
	merge results $sublist
    }
    return $results
}

puts [strandSort {3 1 5 4 2}]
