package require Tcl 8.6

# Term generators; yield list of pairs
proc r2 {} {
    yield {1 1}
    while 1 {yield {2 1}}
}
proc e {} {
    yield {2 1}
    while 1 {yield [list [incr n] $n]}
}
proc pi {} {
    set n 0; set a 3
    while 1 {
	yield [list $a [expr {(2*[incr n]-1)**2}]]
	set a 6
    }
}

# Continued fraction calculator
proc cf {generator {termCount 50}} {
    # Get the chunk of terms we want to work with
    set terms [list [coroutine cf.c $generator]]
    while {[llength $terms] < $termCount} {
	lappend terms [cf.c]
    }
    rename cf.c {}

    # Merge the terms to compute the result
    set val 0.0
    foreach pair [lreverse $terms] {
	lassign $pair a b
	set val [expr {$a + $b/$val}]
    }
    return $val
}

# Demonstration
puts [cf r2]
puts [cf e]
puts [cf pi 250]; # Converges more slowly
