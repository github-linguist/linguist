proc int2bits {n} {
    binary scan [binary format I1 $n] B* binstring
    return [split [string trimleft $binstring 0] ""]

    # another method
    if {$n == 0} {return 0}
    set bits [list]
    while {$n > 0} {
        lappend bits [expr {$n % 2}]
        set n [expr {$n / 2}]
    }
    return [lreverse $bits]
}

proc trial_factor {base exp mod} {
    set square 1
    foreach bit [int2bits $exp] {
        set square [expr {($square ** 2) * ($bit == 1 ? $base : 1) % $mod}]
    }
    return [expr {$square == 1}]
}

proc m_factor p {
    set limit [expr {sqrt(2**$p - 1)}]
    for {set k 1} {2 * $k * $p - 1 < $limit} {incr k} {
        set q [expr {2 * $k * $p + 1}]
        if { ! [primes::is_prime $q]} {
            continue
        } elseif { ! ($q % 8 == 1 || $q % 8 == 7)} {
            # optimization
            continue
        } elseif {[trial_factor 2 $p $q]} {
            # $q is a factor of 2**$p-1
            return $q
        }
    }
    return -1
}

set exp 929
if {[set fact [m_factor 929]] > 0} {
    puts "M$exp has a factor: $fact"
} else {
    puts "no factor found for M$exp"
}
