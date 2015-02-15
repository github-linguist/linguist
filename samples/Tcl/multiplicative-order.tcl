package require Tcl 8.5
package require struct::list

proc multOrder {a m} {
    assert {[gcd $a $m] == 1}
    set mofs [list]
    dict for {p e} [factor_num $m] {
        lappend mofs [multOrdr1 $a $p $e]
    }
    return [struct::list fold $mofs 1 lcm]
}

proc multOrdr1 {a p e} {
    set m [expr {$p ** $e}]
    set t [expr {($p - 1) * ($p ** ($e - 1))}]
    set qs [dict create 1 ""]

    dict for {f0 f1} [factor_num $t] {
        dict for {q -} $qs {
            foreach j [range [expr {1 + $f1}]] {
                dict set qs [expr {$q * $f0 ** $j}] ""
            }
        }
    }

    dict for {q -} $qs {
        if {pypow($a, $q, $m) == 1} break
    }
    return $q
}

####################################################
# utility procs
proc assert {condition {message "Assertion failed!"}} {
    if { ! [uplevel 1 [list expr $condition]]} {
        return -code error $message
    }
}

proc gcd {a b} {
    while {$b != 0} {
        lassign [list $b [expr {$a % $b}]] a b
    }
    return $a
}

proc lcm {a b} {
    expr {$a * $b / [gcd $a $b]}
}

proc factor_num {num} {
    primes::restart
    set factors [dict create]
    for {set i [primes::get_next_prime]} {$i <= $num} {} {
        if {$num % $i == 0} {
            dict incr factors $i
            set num [expr {$num / $i}]
            continue
        } elseif {$i*$i > $num} {
            dict incr factors $num
            break
        } else {
            set i [primes::get_next_prime]
        }
    }
    return $factors
}

####################################################
# a range command akin to Python's
proc range args {
    foreach {start stop step} [switch -exact -- [llength $args] {
        1 {concat 0 $args 1}
        2 {concat   $args 1}
        3 {concat   $args  }
        default {error {wrong # of args: should be "range ?start? stop ?step?"}}
    }] break
    if {$step == 0} {error "cannot create a range when step == 0"}
    set range [list]
    while {$step > 0 ? $start < $stop : $stop < $start} {
        lappend range $start
        incr start $step
    }
    return $range
}

# python's pow()
proc ::tcl::mathfunc::pypow {x y {z ""}} {
    expr {$z eq "" ? $x ** $y : ($x ** $y) % $z}
}

####################################################
# prime number generator
# ref http://wiki.tcl.tk/5996
####################################################
namespace eval primes {}

proc primes::reset {} {
    variable list [list]
    variable current_index end
}

namespace eval primes {reset}

proc primes::restart {} {
    variable list
    variable current_index
    if {[llength $list] > 0} {
        set current_index 0
    }
}

proc primes::is_prime {candidate} {
    variable list

    foreach prime $list {
        if {$candidate % $prime == 0} {
            return false
        }
        if {$prime * $prime > $candidate} {
            return true
        }
    }
    while true {
        set largest [get_next_prime]
        if {$largest * $largest >= $candidate} {
            return [is_prime $candidate]
        }
    }
}

proc primes::get_next_prime {} {
    variable list
    variable current_index

    if {$current_index ne "end"} {
        set p [lindex $list $current_index]
        if {[incr current_index] == [llength $list]} {
            set current_index end
        }
        return $p
    }

    switch -exact -- [llength $list] {
        0 {set candidate 2}
        1 {set candidate 3}
        default {
            set candidate [lindex $list end]
            while true {
                incr candidate 2
                if {[is_prime $candidate]} break
            }
        }
    }
    lappend list $candidate
    return $candidate
}

####################################################
puts [multOrder 37 1000] ;# 100

set b [expr {10**20 - 1}]
puts [multOrder 2 $b] ;# 3748806900
puts [multOrder 17 $b] ;# 1499522760

set a 54
set m 100001
puts [set n [multOrder $a $m]] ;# 9090
puts [expr {pypow($a, $n, $m)}] ;# 1

set lambda {{a n m} {expr {pypow($a, $n, $m) == 1}}}
foreach r [lreverse [range 1 $n]] {
    if {[apply $lambda $a $r $m]} {
        error "Oops, $n is not the smallest:  {$a $r $m} satisfies $lambda"
    }
    if {$r % 1000 == 0} {puts "$r ..."}
}
puts "OK, $n is the smallest n such that {$a $n $m} satisfies $lambda"
