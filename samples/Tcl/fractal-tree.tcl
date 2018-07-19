package require Tk

set SIZE	800
set SCALE	4.0
set BRANCHES	14
set ROTATION_SCALE 0.85
set INITIAL_LENGTH 50.0

proc draw_tree {w x y dx dy size theta depth} {
    global SCALE ROTATION_SCALE
    $w create line $x $y [expr {$x + $dx*$size}] [expr {$y + $dy*$size}]
    if {[incr depth -1] >= 0} {
	set x [expr {$x + $dx*$size}]
	set y [expr {$y + $dy*$size}]
	set ntheta [expr {$theta * $ROTATION_SCALE}]

	# Draw left branch
	draw_tree $w $x $y \
	    [expr {$dx*cos($theta) + $dy*sin($theta)}] \
	    [expr {$dy*cos($theta) - $dx*sin($theta)}] \
	    [expr {$size * (rand() + $SCALE - 1) / $SCALE}] $ntheta $depth
	# Draw right branch
	draw_tree $w $x $y \
	    [expr {$dx*cos(-$theta) + $dy*sin(-$theta)}] \
	    [expr {$dy*cos(-$theta) - $dx*sin(-$theta)}] \
	    [expr {$size * (rand() + $SCALE - 1) / $SCALE}] $ntheta $depth
    }
}

pack [canvas .c -width $SIZE -height $SIZE]
draw_tree .c [expr {$SIZE/2}] [expr {$SIZE-10}] 0.0 -1.0 $INITIAL_LENGTH \
    [expr {3.1415927 / 8}] $BRANCHES
