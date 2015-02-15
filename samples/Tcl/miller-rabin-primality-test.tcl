package require Tcl 8.5

proc miller_rabin {n k} {
    if {$n <= 3} {return true}
    if {$n % 2 == 0} {return false}

    # write n - 1 as 2^s·d with d odd by factoring powers of 2 from n − 1
    set d [expr {$n - 1}]
    set s 0
    while {$d % 2 == 0} {
        set d [expr {$d / 2}]
        incr s
    }

    while {$k > 0} {
        incr k -1
        set a [expr {2 + int(rand()*($n - 4))}]
        set x [expr {($a ** $d) % $n}]
        if {$x == 1 || $x == $n - 1} continue
        for {set r 1} {$r < $s} {incr r} {
            set x [expr {($x ** 2) % $n}]
            if {$x == 1} {return false}
            if {$x == $n - 1} break
        }
	if {$x != $n-1} {return false}
    }
    return true
}

for {set i 1} {$i < 1000} {incr i} {
    if {[miller_rabin $i 10]} {
        puts $i
    }
}
