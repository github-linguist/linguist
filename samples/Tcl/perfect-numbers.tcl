proc perfect n {
    set sum 0
    for {set i 1} {$i <= $n} {incr i} {
        if {$n % $i == 0} {incr sum $i}
    }
    expr {$sum == 2*$n}
}
