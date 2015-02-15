package require Tcl 8.6

proc powers m {
    yield
    for {set n 0} true {incr n} {
	yield [expr {$n ** $m}]
    }
}
coroutine squares powers 2
coroutine cubes powers 3
coroutine filtered apply {{s1 s2} {
    yield
    set f [$s2]
    set v [$s1]
    while true {
	if {$v > $f} {
	    set f [$s2]
	    continue
	} elseif {$v < $f} {
	    yield $v
	}
	set v [$s1]
    }
}} squares cubes

# Drop 20
for {set i 0} {$i<20} {incr i} {filtered}
# Take/print 10
for {} {$i<30} {incr i} {
    puts [filtered]
}
