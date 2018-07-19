proc comb {m n} {
    set set [list]
    for {set i 0} {$i < $n} {incr i} {lappend set $i}
    return [combinations $set $m]
}
proc combinations {list size} {
    if {$size == 0} {
        return [list [list]]
    }
    set retval {}
    for {set i 0} {($i + $size) <= [llength $list]} {incr i} {
        set firstElement [lindex $list $i]
        set remainingElements [lrange $list [expr {$i + 1}] end]
        foreach subset [combinations $remainingElements [expr {$size - 1}]] {
            lappend retval [linsert $subset 0 $firstElement]
        }
    }
    return $retval
}

comb 3 5 ;# ==> {0 1 2} {0 1 3} {0 1 4} {0 2 3} {0 2 4} {0 3 4} {1 2 3} {1 2 4} {1 3 4} {2 3 4}
