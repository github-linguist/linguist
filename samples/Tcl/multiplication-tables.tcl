puts "  x\u2502   1   2   3   4   5   6   7   8   9  10  11  12"
puts \u0020\u2500\u2500\u253c[string repeat \u2500 48]
for {set i 1} {$i <= 12} {incr i} {
    puts -nonewline [format "%3d" $i]\u2502[string repeat " " [expr {$i*4-4}]]
    for {set j 1} {$j <= 12} {incr j} {
	if {$j >= $i} {
	    puts -nonewline [format "%4d" [expr {$i*$j}]]
	}
    }
    puts ""
}
