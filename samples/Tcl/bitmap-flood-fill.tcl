package require Tcl 8.5
package require Tk
package require struct::queue

proc floodFill {img colour point} {
    set new [colour2rgb $colour]
    set old [getPixel $img $point]
    struct::queue Q
    Q put $point
    while {[Q size] > 0} {
        set p [Q get]
        if {[getPixel $img $p] eq $old} {
            set w [findBorder $img $p $old west]
            set e [findBorder $img $p $old east]
            drawLine $img $new $w $e
            set q $w
            while {[x $q] <= [x $e]} {
                set n [neighbour $q north]
                if {[getPixel $img $n] eq $old} {Q put $n}
                set s [neighbour $q south]
                if {[getPixel $img $s] eq $old} {Q put $s}
                set q [neighbour $q east]
            }
        }
    }
    Q destroy
}

proc findBorder {img p colour dir} {
    set lookahead [neighbour $p $dir]
    while {[getPixel $img $lookahead] eq $colour} {
        set p $lookahead
        set lookahead [neighbour $p $dir]
    }
    return $p
}

proc x p {lindex $p 0}
proc y p {lindex $p 1}

proc neighbour {p dir} {
    lassign $p x y
    switch -exact -- $dir {
        west  {return [list [incr x -1] $y]}
        east  {return [list [incr x] $y]}
        north {return [list $x [incr y -1]]}
        south {return [list $x [incr y]]}
    }
}

proc colour2rgb {color_name} {
    foreach part [winfo rgb . $color_name] {
        append colour [format %02x [expr {$part >> 8}]]
    }
    return #$colour
}

set img [newImage 70 50]
fill $img white

drawLine $img blue {0 0} {0 25}
drawLine $img blue {0 25} {35 25}
drawLine $img blue {35 25} {35 0}
drawLine $img blue {35 0} {0 0}
floodFill $img yellow {3 3}

drawCircle $img black {35 25} 24
drawCircle $img black {35 25} 10
floodFill $img orange {34 5}
floodFill $img red {36 5}

toplevel .flood
label .flood.l -image $img
pack .flood.l
