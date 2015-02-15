proc zigzag {size} {
    set m [lrepeat $size [lrepeat $size .]]
    set x 0; set dx -1
    set y 0; set dy 1

    for {set i 0} {$i < $size ** 2} {incr i} {
        if {$x >= $size} {
            incr x -1
            incr y 2
            negate dx dy
        } elseif {$y >= $size} {
            incr x 2
            incr y -1
            negate dx dy
        } elseif {$x < 0 && $y >= 0} {
            incr x
            negate dx dy
        } elseif {$x >= 0 && $y < 0} {
            incr y
            negate dx dy
        }
        lset m $x $y $i
        incr x $dx
        incr y $dy
    }
    return $m
}

proc negate {args} {
    foreach varname $args {
        upvar 1 $varname var
        set var [expr {-1 * $var}]
    }
}

print_matrix [zigzag 5]
