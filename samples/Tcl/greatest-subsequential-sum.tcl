package require Tcl 8.5
set a {-1 -2 3 5 6 -2 -1 4 -4 2 -1}

# from the Perl solution
proc maxsumseq1 {a} {
    set len [llength $a]
    set maxsum 0

    for {set start 0} {$start < $len} {incr start} {
        for {set end $start} {$end < $len} {incr end} {
            set sum 0
            incr sum [expr [join [lrange $a $start $end] +]]
            if {$sum > $maxsum} {
                set maxsum $sum
                set maxsumseq [lrange $a $start $end]
            }
        }
    }
    return $maxsumseq
}

# from the Python solution
proc maxsumseq2 {sequence} {
    set start -1
    set end -1
    set maxsum_ 0
    set sum_ 0
    for {set i 0} {$i < [llength $sequence]} {incr i} {
        set x [lindex $sequence $i]
        incr sum_ $x
        if {$maxsum_ < $sum_} {
            set maxsum_ $sum_
            set end $i
        } elseif {$sum_ < 0} {
            set sum_ 0
            set start $i
        }
    }
    assert {$maxsum_ == [maxsum $sequence]}
    assert {$maxsum_ == [sum [lrange $sequence [expr {$start + 1}] $end]]}
    return [lrange $sequence [expr {$start + 1}] $end]
}

proc maxsum {sequence} {
    set maxsofar 0
    set maxendinghere 0
    foreach x $sequence {
        set maxendinghere [expr {max($maxendinghere + $x, 0)}]
        set maxsofar [expr {max($maxsofar, $maxendinghere)}]
    }
    return $maxsofar
}

proc assert {condition {message "Assertion failed!"}} {
    if { ! [uplevel 1 [list expr $condition]]} {
        return -code error $message
    }
}

proc sum list {
    expr [join $list +]
}


puts "sequence:  $a"
puts "maxsumseq1: [maxsumseq1 $a]"
puts [time {maxsumseq1 $a} 1000]
puts "maxsumseq2: [maxsumseq2 $a]"
puts [time {maxsumseq2 $a} 1000]
