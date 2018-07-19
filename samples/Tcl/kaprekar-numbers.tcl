package require Tcl 8.5;   # Arbitrary precision arithmetic, for stretch goal only
proc kaprekar n {
    if {$n == 1} {return 1}
    set s [expr {$n * $n}]
    for {set i 1} {$i < [string length $s]} {incr i} {
	scan $s "%${i}d%d" a b
	if {$b && $n == $a + $b} {
	    return 1
	    #return [list 1 $a $b]
	}
    }
    return 0
}

# Base goal
for {set i 1} {$i < 10000} {incr i} {
    if {[kaprekar $i]} {lappend klist $i}
}
puts [join $klist ", "]

# Stretch goal
for {set i 1} {$i < 1000000} {incr i} {
    incr kcount [kaprekar $i]
}
puts "$kcount Kaprekar numbers less than 1000000"
