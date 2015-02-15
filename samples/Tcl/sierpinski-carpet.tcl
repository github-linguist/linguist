package require Tcl 8.5

proc map {lambda list} {
    foreach elem $list {
        lappend result [apply $lambda $elem]
    }
    return $result
}

proc sierpinski_carpet n {
    set carpet [list "#"]
    for {set i 1} {$i <= $n} {incr i} {
        set carpet [concat \
            [map {x {subst {$x$x$x}}} $carpet] \
            [map {x {subst {$x[string map {"#" " "} $x]$x}}} $carpet] \
            [map {x {subst {$x$x$x}}} $carpet] \
        ]
    }
    return [join $carpet \n]
}

puts [sierpinski_carpet 3]
