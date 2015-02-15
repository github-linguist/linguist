package require Tk

proc mandelIters {cx cy} {
    set x [set y 0.0]
    for {set count 0} {hypot($x,$y) < 2 && $count < 255} {incr count} {
        set x1 [expr {$x*$x - $y*$y + $cx}]
        set y1 [expr {2*$x*$y + $cy}]
        set x $x1; set y $y1
    }
    return $count
}
proc mandelColor {iter} {
    set r [expr {16*($iter % 15)}]
    set g [expr {32*($iter % 7)}]
    set b [expr {8*($iter % 31)}]
    format "#%02x%02x%02x" $r $g $b
}
image create photo mandel -width 300 -height 300
# Build picture in strips, updating as we go so we have "progress" monitoring
# Also set the cursor to tell the user to wait while we work.
pack [label .mandel -image mandel -cursor watch]
update
for {set x 0} {$x < 300} {incr x} {
    for {set y 0} {$y < 300} {incr y} {
        set i [mandelIters [expr {($x-220)/100.}] [expr {($y-150)/90.}]]
        mandel put [mandelColor $i] -to $x $y
    }
    update
}
.mandel configure -cursor {}
