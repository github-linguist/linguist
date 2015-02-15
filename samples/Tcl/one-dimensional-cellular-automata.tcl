proc evolve {a} {
    set new [list]
    for {set i 0} {$i < [llength $a]} {incr i} {
        lappend new [fate $a $i]
    }
    return $new
}

proc fate {a i} {
    return [expr {[sum $a $i] == 2}]
}

proc sum {a i} {
    set sum 0
    set start [expr {$i - 1 < 0 ? 0 : $i - 1}]
    set end [expr {$i + 1 >= [llength $a] ? $i : $i + 1}]
    for {set j $start} {$j <= $end} {incr j} {
        incr sum [lindex $a $j]
    }
    return $sum
}

proc print {a} {
    puts [string map {0 _ 1 #} [join $a ""]]
}

proc parse {s} {
    return [split [string map {_ 0 # 1} $s] ""]
}

set array [parse "_###_##_#_#_#_#__#__"]
print $array
while {[set new [evolve $array]] ne $array} {
    set array $new
    print $array
}
