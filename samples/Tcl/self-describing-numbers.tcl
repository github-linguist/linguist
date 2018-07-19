package require Tcl 8.5
proc isSelfDescribing num {
    set digits [split $num ""]
    set len [llength $digits]
    set count [lrepeat $len 0]
    foreach d $digits {
	if {$d >= $len} {return false}
	lset count $d [expr {[lindex $count $d] + 1}]
    }
    foreach d $digits c $count {if {$c != $d} {return false}}
    return true
}

for {set i 0} {$i < 100000000} {incr i} {
    if {[isSelfDescribing $i]} {puts $i}
}
