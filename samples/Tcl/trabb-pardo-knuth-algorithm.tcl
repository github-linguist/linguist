# Helper procedures
proc f {x} {expr {abs($x)**0.5 + 5*$x**3}}
proc overflow {y} {expr {$y > 400}}

# Read in 11 numbers, with nice prompting
fconfigure stdout -buffering none
for {set n 1} {$n <= 11} {incr n} {
    puts -nonewline "number ${n}: "
    lappend S [scan [gets stdin] "%f"]
}

# Process and print results in reverse order
foreach x [lreverse $S] {
    set result [f $x]
    if {[overflow $result]} {
	puts "${x}: TOO LARGE!"
    } else {
	puts "${x}: $result"
    }
}
