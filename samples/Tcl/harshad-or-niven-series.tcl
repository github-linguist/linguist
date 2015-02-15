# Determine if the given number is a member of the class of Harshad numbers
proc isHarshad {n} {
    if {$n < 1} {return false}
    set sum [tcl::mathop::+ {*}[split $n ""]]
    return [expr {$n%$sum == 0}]
}

# Get the first 20 numbers that satisfy the condition
for {set n 1; set harshads {}} {[llength $harshads] < 20} {incr n} {
    if {[isHarshad $n]} {
	lappend harshads $n
    }
}
puts [format "First twenty Harshads: %s" [join $harshads ", "]]

# Get the first value greater than 1000 that satisfies the condition
for {set n 1000} {![isHarshad [incr n]]} {} {}
puts "First Harshad > 1000 = $n"
