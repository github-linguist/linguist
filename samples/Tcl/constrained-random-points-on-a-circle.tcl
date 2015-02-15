package require Tcl 8.5

# Generate random point at specified distance from the centre
proc getPoint {range from to} {
    set r2 [expr {$range / 2}]
    set f2 [expr {$from ** 2}]
    set t2 [expr {$to ** 2}]
    while 1 {
	set x [expr {int($range * rand())}]
	set y [expr {int($range * rand())}]
	set d2 [expr {($x-$r2)**2 + ($y-$r2)**2}]
	if {$d2 >= $f2 && $d2 <= $t2} {
	    return [list $y $x]
	}
    }
}

# Make somewhere to store the counters
set ary [lrepeat 31 [lrepeat 31 0]]

# Generate 100 random points
for {set i 0} {$i < 100} {incr i} {
    set location [getPoint 31 10 15]
    # Increment the counter for the point
    lset ary $location [expr {1 + [lindex $ary $location]}]
}

# Simple renderer
foreach line $ary {
    foreach c $line {
	puts -nonewline [expr {$c == 0 ? " " : $c > 9 ? "X" : $c}]
    }
    puts ""
}
