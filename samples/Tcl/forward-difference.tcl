proc do_fwd_diff {list} {
    set previous [lindex $list 0]
    set new [list]
    foreach current [lrange $list 1 end] {
        lappend new [expr {$current - $previous}]
        set previous $current
    }
    return $new
}

proc fwd_diff {list order} {
    while {$order >= 1} {
        set list [do_fwd_diff $list]
        incr order -1
    }
    return $list
}

set a {90.5 47 58 29 22 32 55 5 55 73.5}

for {set order 0} {$order <= 10} {incr order} {
    puts [format "%d\t%s" $order [fwd_diff $a $order]]
}
