package require Tcl 8.5
package require Tk

proc ::tcl::mathfunc::ipart x {expr {int($x)}}
proc ::tcl::mathfunc::fpart x {expr {$x - int($x)}}
proc ::tcl::mathfunc::rfpart x {expr {1.0 - fpart($x)}}

proc drawAntialiasedLine {image colour p1 p2} {
    lassign $p1 x1 y1
    lassign $p2 x2 y2

    set steep [expr {abs($y2 - $y1) > abs($x2 - $x1)}]
    if {$steep} {
        lassign [list $x1 $y1] y1 x1
        lassign [list $x2 $y2] y2 x2
    }
    if {$x1 > $x2} {
        lassign [list $x1 $x2] x2 x1
        lassign [list $y1 $y2] y2 y1
    }
    set deltax [expr {$x2 - $x1}]
    set deltay [expr {abs($y2 - $y1)}]
    set gradient [expr {1.0 * $deltay / $deltax}]

    # handle the first endpoint
    set xend [expr {round($x1)}]
    set yend [expr {$y1 + $gradient * ($xend - $x1)}]
    set xgap [expr {rfpart($x1 + 0.5)}]
    set xpxl1 $xend
    set ypxl1 [expr {ipart($yend)}]
    plot $image $colour $steep $xpxl1 $ypxl1 [expr {rfpart($yend)*$xgap}]
    plot $image $colour $steep $xpxl1 [expr {$ypxl1+1}] [expr {fpart($yend)*$xgap}]
    set itery [expr {$yend + $gradient}]

    # handle the second endpoint
    set xend [expr {round($x2)}]
    set yend [expr {$y2 + $gradient * ($xend - $x2)}]
    set xgap [expr {rfpart($x2 + 0.5)}]
    set xpxl2 $xend
    set ypxl2 [expr {ipart($yend)}]
    plot $image $colour $steep $xpxl2 $ypxl2 [expr {rfpart($yend)*$xgap}]
    plot $image $colour $steep $xpxl2 [expr {$ypxl2+1}] [expr {fpart($yend)*$xgap}]

    for {set x [expr {$xpxl1 + 1}]} {$x < $xpxl2} {incr x} {
        plot $image $colour $steep $x [expr {ipart($itery)}] [expr {rfpart($itery)}]
        plot $image $colour $steep $x [expr {ipart($itery) + 1}] [expr {fpart($itery)}]
        set itery [expr {$itery + $gradient}]
    }
}

proc plot {image colour steep x y c} {
    set point [expr {$steep ? [list $y $x] : [list $x $y]}]
    set newColour [antialias $colour [getPixel $image $point] $c]
    setPixel $image $newColour $point
}

proc antialias {newColour oldColour c} {
    # get the new colour r,g,b
    if {[scan $newColour "#%2x%2x%2x%c" nr ng gb -] != 3} {
        scan [colour2rgb $newColour] "#%2x%2x%2x" nr ng nb
    }

    # get the current colour r,g,b
    scan $oldColour "#%2x%2x%2x" cr cg cb

    # blend the colours in the ratio defined by "c"
    foreach new [list $nr $ng $nb] curr [list $cr $cg $cb] {
        append blend [format {%02x} [expr {round($new*$c + $curr*(1.0-$c))}]]
    }
    return #$blend
}

proc colour2rgb {color_name} {
    foreach part [winfo rgb . $color_name] {
        append colour [format %02x [expr {$part >> 8}]]
    }
    return #$colour
}

set img [newImage 500 500]
fill $img blue
for {set a 10} {$a < 500} {incr a 60} {
    drawAntialiasedLine $img yellow {10 10} [list 490 $a]
    drawAntialiasedLine $img yellow {10 10} [list $a 490]
}
toplevel .wu
label .wu.l -image $img
pack .wu.l
