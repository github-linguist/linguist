package require Tcl 8.5

proc sieve n {
    if {$n < 2} {return {}}

    # create a container to hold the sequence of numbers.
    # use a dictionary for its speedy access (like an associative array)
    # and for its insertion order preservation (like a list)
    set nums [dict create]
    for {set i 2} {$i <= $n} {incr i} {
        # the actual value is never used
        dict set nums $i ""
    }

    set primes [list]
    while {[set nextPrime [lindex [dict keys $nums] 0]] <= sqrt($n)} {
        dict unset nums $nextPrime
        for {set i [expr {$nextPrime ** 2}]} {$i <= $n} {incr i $nextPrime} {
            dict unset nums $i
        }
        lappend primes $nextPrime
    }
    return [concat $primes [dict keys $nums]]
}

puts [sieve 100]   ;# 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
