proc combsort {input} {
    set gap [llength $input]
    while 1 {
	set gap [expr {int(floor($gap / 1.3))}]
	set swaps 0
	for {set i 0} {$i+$gap < [llength $input]} {incr i} {
	    set j [expr {$i+$gap}]
	    if {[lindex $input $i] > [lindex $input $j]} {
		set tmp [lindex $input $i]
		lset input $i [lindex $input $j]
		lset input $j $tmp
		incr swaps
	    }
	}
	if {$gap <= 1 && !$swaps} break
    }
    return $input
}

set data {23 76 99 58 97 57 35 89 51 38 95 92 24 46 31 24 14 12 57 78}
puts [combsort $data]
