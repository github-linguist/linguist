package require struct::list

proc swap {listVar} {
    upvar 1 $listVar list
    set n [lindex $list 0]
    for {set i 0; set j [expr {$n-1}]} {$i<$j} {incr i;incr j -1} {
	set tmp [lindex $list $i]
	lset list $i [lindex $list $j]
	lset list $j $tmp
    }
}

proc swaps {list} {
    for {set i 0} {[lindex $list 0] > 1} {incr i} {
	swap list
    }
    return $i
}

proc topswops list {
    set n 0
    ::struct::list foreachperm p $list {
	set n [expr {max($n,[swaps $p])}]
    }
    return $n
}

proc topswopsTo n {
    puts "n\ttopswops(n)"
    for {set i 1} {$i <= $n} {incr i} {
	puts $i\t[topswops [lappend list $i]]
    }
}
topswopsTo 10
