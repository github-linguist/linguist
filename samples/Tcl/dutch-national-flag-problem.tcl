# The comparison function
proc dutchflagcompare {a b} {
    set colors {red white blue}
    return [expr {[lsearch $colors $a] - [lsearch $colors $b]}]
}

# The test function (evil shimmer of list to string!)
proc isFlagSorted lst {
    expr {![regexp {blue.*(white|red)} $lst] && ![regexp {white.*red} $lst]}
}

# A ball generator
proc generateBalls n {
    for {set i 0} {$i<$n} {incr i} {
	lappend result [lindex {red white blue} [expr {int(rand()*3)}]]
    }
    return $result
}

# Do the challenge with 20 balls
set balls [generateBalls 20]
if {[isFlagSorted $balls]} {
    error "already a sorted flag"
}
set sorted [lsort -command dutchflagcompare $balls]
if {[isFlagSorted $sorted]} {
    puts "Sorted the flag\n$sorted"
} else {
    puts "sort failed\n$sorted"
}
