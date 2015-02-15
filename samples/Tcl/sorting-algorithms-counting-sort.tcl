proc countingsort {a {min ""} {max ""}} {
    # If either of min or max weren't given, compute them now
    if {$min eq ""} {
        set min [::tcl::mathfunc::min $a]
    }
    if {$max eq ""} {
        set max [::tcl::mathfunc::max $a]
    }

    # Make the "array" of counters
    set count [lrepeat [expr {$max - $min + 1}] 0]

    # Count the values in the input list
    foreach n $a {
        set idx [expr {$n - $min}]
        lincr count $idx
    }

    # Build the output list
    set z 0
    for {set i $min} {$i <= $max} {incr i} {
        set idx [expr {$i - $min}]
        while {[lindex $count $idx] > 0} {
            lset a $z $i
            incr z
            lincr count $idx -1
        }
    }
    return $a
}

# Helper that will increment an existing element of a list
proc lincr {listname idx {value 1}} {
    upvar 1 $listname list
    lset list $idx [expr {[lindex $list $idx] + $value}]
}

# Demo code
for {set i 0} {$i < 50} {incr i} {lappend a [expr {1+ int(rand()*10)}]}
puts $a
puts [countingsort $a]
