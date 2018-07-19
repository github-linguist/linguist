package require Tcl 8.5

proc makeChange {amount coins} {
    set table [lrepeat [expr {$amount+1}] [lrepeat [llength $coins] {}]]
    lset table 0 [lrepeat [llength $coins] 1]
    for {set i 1} {$i <= $amount} {incr i} {
	for {set j 0} {$j < [llength $coins]} {incr j} {
	    set k [expr {$i - [lindex $coins $j]}]
	    lset table $i $j [expr {
		($k < 0 ? 0 : [lindex $table $k $j]) +
		($j < 1 ? 0 : [lindex $table $i [expr {$j-1}]])
	    }]
	}
    }
    return [lindex $table end end]
}

puts [makeChange 100 {1 5 10 25}]
puts [makeChange 100000 {1 5 10 25 50 100}]
# Making change with the EU coin set:
puts [makeChange 100 {1 2 5 10 20 50 100 200}]
puts [makeChange 100000 {1 2 5 10 20 50 100 200}]
