package require Tcl 8.5

proc map {lambda list} {
    foreach elem $list {
        lappend result [apply $lambda $elem]
    }
    return $result
}

proc sierpinski_triangle n {
    set down [list *]
    set space " "
    for {set i 1} {$i <= $n} {incr i} {
        set down [concat \
            [map [subst -nocommands {x {expr {"$space[set x]$space"}}}] $down] \
            [map {x {expr {"$x $x"}}} $down] \
        ]
        append space $space
    }
    return [join $down \n]
}

puts [sierpinski_triangle 4]
