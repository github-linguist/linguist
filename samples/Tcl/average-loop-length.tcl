# Generate a list of the numbers increasing from $a to $b
proc range {a b} {
    for {set result {}} {$a <= $b} {incr a} {lappend result $a}
    return $result
}

# Computing the expected value analytically
proc tcl::mathfunc::factorial n {
    ::tcl::mathop::* {*}[range 2 $n]
}
proc Analytical {n} {
    set sum 0.0
    foreach x [range 1 $n] {
	set sum [expr {$sum + factorial($n) / factorial($n-$x) / double($n)**$x}]
    }
    return $sum
}

# Determining an approximation to the value experimentally
proc Experimental {n numTests} {
    set count 0
    set u0 [lrepeat $n 1]
    foreach run [range 1 $numTests] {
	set unseen $u0
	for {set i 0} {[lindex $unseen $i]} {incr count} {
	    lset unseen $i 0
	    set i [expr {int(rand()*$n)}]
	}
    }
    return [expr {$count / double($numTests)}]
}

# Tabulate the results in exactly the original format
puts " N    average    analytical    (error)"
puts "===  =========  ============  ========="
foreach n [range 1 20] {
    set a [Analytical $n]
    set e [Experimental $n 100000]
    puts [format "%3d  %9.4f  %12.4f  (%6.2f%%)" $n $e $a [expr {abs($e-$a)/$a*100.0}]]
}
