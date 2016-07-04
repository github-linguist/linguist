package require Tk
# Allocate a temporary drawing surface
canvas .c
# Draw the output we want
for {set y 0;set dx 1} {$y < 11*72} {incr y 72;incr dx} {
    for {set x 0;set c 0} {$x < 8.5*72} {incr x $dx;set c [expr {!$c}]} {
	.c create rectangle $x $y [expr {$x+$dx+1}] [expr {$y+73}] \
	    -fill [lindex {black white} $c] -outline {}
    }
}
# Send postscript to default printer, scaled 1 pixel -> 1 point
exec lp - << [.c postscript -height $y -width $x -pageheight $y -pagewidth $x]
# Explicit exit; no GUI desired
exit
