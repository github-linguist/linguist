proc int2words {n} {
    if { ! [regexp -- {^(-?\d+)$} $n -> n]} {
        error "not a decimal integer"
    }
    if {$n == 0} {
        return zero
    }
    if {$n < 0} {
        return "negative [int2words [expr {abs($n)}]]"
    }
    if {[string length $n] > 36} {
        error "value too large to represent"
    }

    set groups [get_groups $n]
    set l [llength $groups]
    foreach group $groups {
        incr l -1
        # ensure any group with a leading zero is not treated as octal
        set val [scan $group %d]
        if {$val > 0} {
            lappend result [group2words $val $l]
        }
    }
    return [join $result ", "]
}

set small {"" one two three four five six seven eight nine ten eleven twelve
           thirteen fourteen fifteen sixteen seventeen eighteen nineteen}
set tens {"" "" twenty thirty forty fifty sixty seventy eighty ninety}
set powers {"" thousand}
foreach p {m b tr quadr quint sext sept oct non dec} {lappend powers ${p}illion}

proc group2words {n level} {
    global small tens powers
    if {$n < 20} {
        lappend result [lindex $small $n]
    } elseif {$n < 100} {
        lassign [divmod $n 10] a b
        set result [lindex $tens $a]
        if {$b > 0} {
            append result - [lindex $small $b]
        }
    } else {
        lassign [divmod $n 100] a b
        lappend result [lindex $small $a] hundred
        if {$b > 0} {
            lappend result and [group2words $b 0]
        }
    }
    return [join [concat $result [lindex $powers $level]]]
}

proc divmod {n d} {
    return [list [expr {$n / $d}] [expr {$n % $d}]]
}

proc get_groups {num} {
    # from http://wiki.tcl.tk/5000
    while {[regsub {^([-+]?\d+)(\d\d\d)} $num {\1 \2} num]} {}
    return [split $num]
}

foreach test {
        0 -0 5 -5 10 25 99 100 101 999 1000 1008 1010 54321 1234567890
        0x7F
        123456789012345678901234567890123456
        1234567890123456789012345678901234567
} {
    catch {int2words $test} result
    puts "$test -> $result"
}
