proc floydTriangle n {
    # Compute the column widths
    for {set i [expr {$n*($n-1)/2+1}]} {$i <= $n*($n+1)/2} {incr i} {
	lappend w [string length $i]
    }
    # Print the triangle
    for {set i 0; set j 1} {$j <= $n} {incr j} {
	for {set p -1; set k 0} {$k < $j} {incr k} {
	    puts -nonewline [format "%*d " [lindex $w [incr p]] [incr i]]
	}
	puts ""
    }
}

# Demonstration
puts "Floyd 5:"
floydTriangle 5
puts "Floyd 14:"
floydTriangle 14
