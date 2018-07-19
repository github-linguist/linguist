package require Tcl 8.5

proc main {} {
    evolve 3 blinker [initialize_tableau {3 3} {{0 1} {1 1} {2 1}}]
    evolve 5 glider  [initialize_tableau {4 4} {{0 1} {1 2} {2 0} {2 1} {2 2}}]
}

proc evolve {generations name tableau} {
    for {set gen 1} {$gen <= $generations} {incr gen} {
        puts "$name generation $gen:"
        print $tableau
        set tableau [next_generation $tableau]
    }
    puts ""
}

proc initialize_tableau {size initial_life} {
    lassign $size ::max_x ::max_y
    set tableau [blank_tableau]
    foreach point $initial_life {
        lset tableau {*}$point 1
    }
    return $tableau
}

proc blank_tableau {} {
    return [lrepeat $::max_x [lrepeat $::max_y 0]]
}

proc print {tableau} {
    foreach row $tableau {puts [string map {0 . 1 #} [join $row]]}
}

proc next_generation {tableau} {
    set new [blank_tableau]
    for {set x 0} {$x < $::max_x} {incr x} {
        for {set y 0} {$y < $::max_y} {incr y} {
            lset new $x $y [fate [list $x $y] $tableau]
        }
    }
    return $new
}

proc fate {point tableau} {
    set current [value $point $tableau]
    set neighbours [sum_neighbours $point $tableau]
    return [expr {($neighbours == 3) || ($neighbours == 2 && $current == 1)}]
}

proc value {point tableau} {
    return [lindex $tableau {*}$point]
}

proc sum_neighbours {point tableau} {
    set sum 0
    foreach neighbour [get_neighbours $point] {
        incr sum [value $neighbour $tableau]
    }
    return $sum
}

proc get_neighbours {point} {
    lassign $point x y
    set results [list]
    foreach x_off {-1 0 1} {
        foreach y_off {-1 0 1} {
            if { ! ($x_off == 0 && $y_off == 0)} {
                set i [expr {$x + $x_off}]
                set j [expr {$y + $y_off}]
                if {(0 <= $i && $i < $::max_x) && (0 <= $j && $j < $::max_y)} {
                    lappend results [list $i $j]
                }
            }
        }
    }
    return $results
}

main
