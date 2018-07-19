proc median args {
    set list [lsort -real $args]
    set len [llength $list]
    # Odd number of elements
    if {$len & 1} {
        return [lindex $list [expr {($len-1)/2}]]
    }
    # Even number of elements
    set idx2 [expr {$len/2}]
    set idx1 [expr {$idx2-1}]
    return [expr {
        ([lindex $list $idx1] + [lindex $list $idx2])/2.0
    }]
}

puts [median 3.0 4.0 1.0 -8.4 7.2 4.0 1.0 1.2]; # --> 2.1
