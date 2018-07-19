package require Tcl 8.5
package require Tk

proc drawCircle {image colour point radius} {
    lassign $point x0 y0

    setPixel $image $colour [list $x0 [expr {$y0 + $radius}]]
    setPixel $image $colour [list $x0 [expr {$y0 - $radius}]]
    setPixel $image $colour [list [expr {$x0 + $radius}] $y0]
    setPixel $image $colour [list [expr {$x0 - $radius}] $y0]

    set f [expr {1 - $radius}]
    set ddF_x 1
    set ddF_y [expr {-2 * $radius}]
    set x 0
    set y $radius

    while {$x < $y} {
        assert {$ddF_x == 2 * $x + 1}
        assert {$ddF_y == -2 * $y}
        assert {$f == $x*$x + $y*$y - $radius*$radius + 2*$x - $y + 1}
        if {$f >= 0} {
            incr y -1
            incr ddF_y 2
            incr f $ddF_y
        }
        incr x
        incr ddF_x 2
        incr f $ddF_x
        setPixel $image $colour [list [expr {$x0 + $x}] [expr {$y0 + $y}]]
        setPixel $image $colour [list [expr {$x0 - $x}] [expr {$y0 + $y}]]
        setPixel $image $colour [list [expr {$x0 + $x}] [expr {$y0 - $y}]]
        setPixel $image $colour [list [expr {$x0 - $x}] [expr {$y0 - $y}]]
        setPixel $image $colour [list [expr {$x0 + $y}] [expr {$y0 + $x}]]
        setPixel $image $colour [list [expr {$x0 - $y}] [expr {$y0 + $x}]]
        setPixel $image $colour [list [expr {$x0 + $y}] [expr {$y0 - $x}]]
        setPixel $image $colour [list [expr {$x0 - $y}] [expr {$y0 - $x}]]

    }
}

# create the image and display it
set img [newImage 200 100]
label .l -image $img
pack .l

fill $img black
drawCircle $img blue {100 50} 49
