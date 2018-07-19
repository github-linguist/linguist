package require Tcl 8.5

# Optimized version of the Sieve-of-Eratosthenes task solution
proc sieve n {
    set primes [list]
    if {$n < 2} {return $primes}
    set nums [dict create]
    for {set i 2} {$i <= $n} {incr i} {
        dict set nums $i ""
    }
    set next 2
    set limit [expr {sqrt($n)}]
    while {$next <= $limit} {
        for {set i $next} {$i <= $n} {incr i $next} {dict unset nums $i}
        lappend primes $next
	dict for {next -} $nums break
    }
    return [concat $primes [dict keys $nums]]
}

proc isLeftTruncatable n {
    global isPrime
    while {[string length $n] > 0} {
	if {![info exist isPrime($n)]} {
	    return false
	}
	set n [string range $n 1 end]
    }
    return true
}
proc isRightTruncatable n {
    global isPrime
    while {[string length $n] > 0} {
	if {![info exist isPrime($n)]} {
	    return false
	}
	set n [string range $n 0 end-1]
    }
    return true
}

# Demo code
set limit 1000000
puts "calculating primes up to $limit"
set primes [sieve $limit]
puts "search space contains [llength $primes] members"
foreach p $primes {
    set isPrime($p) "yes"
}
set primes [lreverse $primes]

puts "searching for largest left-truncatable prime"
foreach p $primes {
    if {[isLeftTruncatable $p]} {
	puts FOUND:$p
	break
    }
}

puts "searching for largest right-truncatable prime"
foreach p $primes {
    if {[isRightTruncatable $p]} {
	puts FOUND:$p
	break
    }
}
