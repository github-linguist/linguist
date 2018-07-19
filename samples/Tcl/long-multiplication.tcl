package require Tcl 8.5

proc longmult {x y} {
    set digits [lreverse [split $x ""]]
    set result {0}
    set j -2
    foreach m [lreverse [split $y ""]] {
	set c 0
	set i [incr j]
	foreach d $digits {
	    set v [lindex $result [incr i]]
	    if {$v eq ""} {
		lappend result 0
		set v 0
	    }
	    regexp (.)(.)$ 0[expr {$v + $c + $d*$m}] -> c v
	    lset result $i $v
	}
	lappend result $c
    }
    # Reconvert digit list into a decimal number
    set result [string trimleft [join [lreverse $result] ""] 0]
    if {$result == ""} then {return 0} else {return $result}
}

puts [set n [expr {2**64}]]
puts [longmult $n $n]
puts [expr {$n * $n}]
