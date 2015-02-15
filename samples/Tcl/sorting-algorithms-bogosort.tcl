package require Tcl 8.5

proc shuffleInPlace {listName} {
    upvar 1 $listName list
    set len [set len2 [llength $list]]
    for {set i 0} {$i < $len-1} {incr i; incr len2 -1} {
        # Pick cell to swap with
        set n [expr {int($i + $len2 * rand())}]
        # Perform swap
        set temp [lindex $list $i]
        lset list $i [lindex $list $n]
        lset list $n $temp
    }
}
proc inOrder {list} {
    set prev [lindex $list 0]
    foreach item [lrange $list 1 end] {
        if {$prev > $item} {
            return false
        }
        set prev $item
    }
    return true
}
proc bogosort {list} {
    while { ! [inOrder $list]} {
        shuffleInPlace list
    }
    return $list
}
