package require Tcl 8.5

proc point_in_polygon {point polygon} {
    set count 0
    foreach side [sides $polygon] {
        if {[ray_intersects_line $point $side]} {
            incr count
        }
    }
    expr {$count % 2} ;#-- 1 = odd = true, 0 = even = false
}
proc sides polygon {
    lassign $polygon x0 y0
    foreach {x y} [lrange [lappend polygon $x0 $y0] 2 end] {
        lappend res [list $x0 $y0 $x $y]
        set x0 $x
        set y0 $y
    }
    return $res
}
proc ray_intersects_line {point line} {
    lassign $point Px Py
    lassign $line Ax Ay Bx By
    # Reverse line direction if necessary
    if {$By < $Ay} {
	lassign $line Bx By Ax Ay
    }
    # Add epsilon to
    if {$Py == $Ay || $Py == $By} {
	set Py [expr {$Py + abs($Py)/1e6}]
    }
    # Bounding box checks
    if {$Py < $Ay || $Py > $By || $Px > max($Ax,$Bx)} {
	return 0
    } elseif {$Px < min($Ax,$Bx)} {
	return 1
    }
    # Compare dot products to compare (cosines of) angles
    set mRed [expr {$Ax != $Bx ? ($By-$Ay)/($Bx-$Ax) : Inf}]
    set mBlu [expr {$Ax != $Px ? ($Py-$Ay)/($Px-$Ax) : Inf}]
    return [expr {$mBlu >= $mRed}]
}

foreach {point poly} {
    {0 0}	{-1 -1  -1 1  1 1  1 -1}
    {2 2}	{-1 -1  -1 1  1 1  1 -1}
    {0 0}	{-2 -2  -2 2  2 2  2 -2   2 -1  1 1  -1 1  -1 -1  1 -1  2 -1}
    {1.5 1.5}	{-2 -2  -2 2  2 2  2 -2   2 -1  1 1  -1 1  -1 -1  1 -1  2 -1}
    {5 5}	{0 0  2.5 2.5  0 10  2.5 7.5  7.5 7.5  10 10  10 0  7.5 0.1}
    {5 8}	{0 0  2.5 2.5  0 10  2.5 7.5  7.5 7.5  10 10  10 0  7.5 0.1}
    {2 2}	{0 0  2.5 2.5  0 10  2.5 7.5  7.5 7.5  10 10  10 0  7.5 0.1}
    {0 0}	{0 0  2.5 2.5  0 10  2.5 7.5  7.5 7.5  10 10  10 0  7.5 0.1}
    {10 10}	{0 0  2.5 2.5  0 10  2.5 7.5  7.5 7.5  10 10  10 0  7.5 0.1}
    {2.5 2.5}	{0 0  2.5 2.5  0 10  2.5 7.5  7.5 7.5  10 10  10 0  7.5 0.1}
    {-5 5}	{3 0  7 0  10 5  7 10  3 10  0 5}
} {
    puts "$point in $poly = [point_in_polygon $point $poly]"
}
