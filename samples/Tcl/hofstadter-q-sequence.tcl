package require Tcl 8.5

# Index 0 is not used, but putting it in makes the code a bit shorter
set tcl::mathfunc::Qcache {Q:-> 1 1}
proc tcl::mathfunc::Q {n} {
    variable Qcache
    if {$n >= [llength $Qcache]} {
	lappend Qcache [expr {Q($n - Q($n-1)) + Q($n - Q($n-2))}]
    }
    return [lindex $Qcache $n]
}

# Demonstration code
for {set i 1} {$i <= 10} {incr i} {
    puts "Q($i) == [expr {Q($i)}]"
}
# This runs very close to recursion limit...
puts "Q(1000) == [expr Q(1000)]"
# This code is OK, because the calculations are done step by step
set q [expr Q(1)]
for {set i 2} {$i <= 100000} {incr i} {
    incr count [expr {$q > [set q [expr {Q($i)}]]}]
}
puts "Q(i)<Q(i-1) for i \[2..100000\] is true $count times"
