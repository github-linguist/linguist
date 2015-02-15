package require Tcl 8.5
package require Tk

proc drawLine {image colour point0 point1} {
    lassign $point0 x0 y0
    lassign $point1 x1 y1

    set steep [expr {abs($y1 - $y0) > abs($x1 - $x0)}]
    if {$steep} {
        lassign [list $x0 $y0] y0 x0
        lassign [list $x1 $y1] y1 x1
    }
    if {$x0 > $x1} {
        lassign [list $x0 $x1] x1 x0
        lassign [list $y0 $y1] y1 y0
    }
    set deltax [expr {$x1 - $x0}]
    set deltay [expr {abs($y1 - $y0)}]
    set error [expr {$deltax / 2}]
    set ystep [expr {$y0 < $y1 ? 1 : -1}]

    for {set x $x0; set y $y0} {$x <= $x1} {incr x} {
        setPixel $image $colour [expr {$steep ? [list $y $x] : [list $x $y]}]
        incr error -$deltay
        if {$error < 0} {
            incr y $ystep
            incr error $deltax
        }
    }
}

# create the image and display it
set img [newImage 200 100]
label .l -image $img
pack .l

fill $img black
drawLine $img yellow {20 20} {180 80}
drawLine $img yellow {180 20} {20 80}
