package require Tcl 8.5
package require Tk

proc drawBezier {img colour args} {
    # ensure the points are increasing along the x-axis
    set points [lsort -real -index 0 $args]

    set xmin [x [lindex $points 0]]
    set xmax [x [lindex $points end]]
    set prev [lindex $points 0]
    set increment 2
    for {set x [expr {$xmin + $increment}]} {$x <= $xmax} {incr x $increment} {
        set t [expr {1.0 * ($x - $xmin) / ($xmax - $xmin)}]
        set this [list $x [::tcl::mathfunc::round [bezier $t $points]]]
        drawLine $img $colour $prev $this
        set prev $this
    }
}

# the generalized n-degree Bezier summation
proc bezier {t points} {
    set n [expr {[llength $points] - 1}]
    for {set i 0; set sum 0.0} {$i <= $n} {incr i} {
        set sum [expr {$sum + [C $n $i] * (1-$t)**($n - $i) * $t**$i * [y [lindex $points $i]]}]
    }
    return $sum
}

proc C {n i} {expr {[ifact $n] / ([ifact $i] * [ifact [expr {$n - $i}]])}}
proc ifact n {
    for {set i $n; set sum 1} {$i >= 2} {incr i -1} {
        set sum [expr {$sum * $i}]
    }
    return $sum
}

proc x p {lindex $p 0}
proc y p {lindex $p 1}

proc newbezier {n w} {
    set size 400
    set bezier [newImage $size $size]
    fill $bezier white
    for {set i 1} {$i <= $n} {incr i} {
        set point [list [expr {int($size*rand())}] [expr {int($size*rand())}]]
        lappend points $point
        drawCircle $bezier red $point 3
    }
    puts $points

    drawBezier $bezier blue {*}$points

    $w configure -image $bezier
}

set degree 4 ;# cubic bezier -- for quadratic, use 3
label .img
button .new -command [list newbezier $degree .img] -text New
button .exit -command exit -text Exit
pack .new .img .exit -side top
