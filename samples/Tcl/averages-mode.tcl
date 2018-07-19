# Can find the modal value of any vector of values
proc mode {n args} {
    foreach n [list $n {*}$args] {
        dict incr counter $n
    }
    set counts [lsort -stride 2 -index 1 -decreasing $counter]
    set best {}
    foreach {n count} $counts {
        if {[lindex $counts 1] == $count} {
            lappend best $n
        } else break
    }
    return $best
}

# Testing
puts [mode 1 3 6 6 6 6 7 7 12 12 17];  # --> 6
puts [mode 1 1 2 4 4];  # --> 1 4
