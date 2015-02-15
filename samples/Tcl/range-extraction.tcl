proc rangeExtract list {
    set result [lindex $list 0]
    set first [set last [lindex $list 0]]
    foreach term [lrange $list 1 end] {
	if {$term == $last+1} {
	    set last $term
	    continue
	}
	if {$last > $first} {
	    append result [expr {$last == $first+1 ? "," : "-"}] $last
	}
	append result "," $term
	set first [set last $term]
    }
    if {$last == $first+1} {
	append result "," $last
    } elseif {$last > $first} {
	append result "-" $last
    }
    return $result
}

# Commas already removed so it is a natural Tcl list
puts [rangeExtract {
    0 1 2 4 6 7 8 11 12 14
    15 16 17 18 19 20 21 22 23 24
    25 27 28 29 30 31 32 33 35 36
    37 38 39
}]
