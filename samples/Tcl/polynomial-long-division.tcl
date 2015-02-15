# poldiv - Divide two polynomials n and d.
#          Result is a list of two polynomials, q and r, where n = qd + r
#          and the degree of r is less than the degree of b.
#          Polynomials are represented as lists, where element 0 is the
#          x**0 coefficient, element 1 is the x**1 coefficient, and so on.

proc poldiv {a b} {
    # Toss out leading zero coefficients efficiently
    while {[lindex $a end] == 0} {set a [lrange $a[set a {}] 0 end-1]}
    while {[lindex $b end] == 0} {set b [lrange $b[set b {}] 0 end-1]}
    if {[llength $a] < [llength $b]} {
        return [list 0 $a]
    }

    # Rearrange the terms to put highest powers first
    set n [lreverse $a]
    set d [lreverse $b]

    # Carry out classical long division, accumulating quotient coefficients
    # in q, and replacing n with the remainder.
    set q {}
    while {[llength $n] >= [llength $d]} {
        set qd [expr {[lindex $n 0] / [lindex $d 0]}]
        set i 0
        foreach nd [lrange $n 0 [expr {[llength $d] - 1}]] dd $d {
            lset n $i [expr {$nd - $qd * $dd}]
            incr i
        }
        lappend q $qd
        set n [lrange $n 1 end]
    }

    # Return quotient and remainder, constant term first
    return [list [lreverse $q] [lreverse $n]]
}

# Demonstration
lassign [poldiv {-42. 0. -12. 1.} {-3. 1. 0. 0.}] Q R
puts [list Q = $Q]
puts [list R = $R]
