package require Tcl 8.5

proc tcl::mathfunc::modexp {a b n} {
    for {set c 1} {$b} {set a [expr {$a*$a%$n}]} {
        if {$b & 1} {
            set c [expr {$c*$a%$n}]
        }
        set b [expr {$b >> 1}]
    }
    return $c
}
# Based on Miller-Rabin primality testing, but with small prime check first
proc is_prime {n {count 10}} {
    # fast check against small primes
    foreach p {
	2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
    } {
	if {$n == $p} {return true}
	if {$n % $p == 0} {return false}
    }

    # write n-1 as 2^s·d with d odd by factoring powers of 2 from n-1
    set d [expr {$n - 1}]
    for {set s 0} {$d & 1 == 0} {incr s} {
        set d [expr {$d >> 1}]
    }

    for {} {$count > 0} {incr count -1} {
        set a [expr {2 + int(rand()*($n - 4))}]
        set x [expr {modexp($a, $d, $n)}]
        if {$x == 1 || $x == $n - 1} continue
        for {set r 1} {$r < $s} {incr r} {
            set x [expr {modexp($x, 2, $n)}]
            if {$x == 1} {return false}
            if {$x == $n - 1} break
        }
	if {$x != $n-1} {return false}
    }
    return true
}

proc max_left_truncatable_prime {base} {
    set stems {}
    for {set i 2} {$i < $base} {incr i} {
	if {[is_prime $i]} {
	    lappend stems $i
	}
    }
    set primes $stems
    set size 0
    for {set b $base} {[llength $stems]} {set b [expr {$b * $base}]} {
	# Progress monitoring is nice once we get to 10 and beyond...
	if {$base > 9} {
	    puts "\t[llength $stems] candidates at length [incr size]"
	}
	set primes $stems
	set certainty [expr {[llength $primes] > 100 ? 1 : 5}]
	set stems {}
	foreach s $primes {
	    for {set i 1} {$i < $base} {incr i} {
		set n [expr {$b*$i + $s}]
		if {[is_prime $n $certainty]} {
		    lappend stems $n
		}
	    }
	}
    }
    # Could be several at same length; choose largest
    return [tcl::mathfunc::max {*}$primes]
}

for {set i 3} {$i <= 20} {incr i} {
    puts "$i: [max_left_truncatable_prime $i]"
}
