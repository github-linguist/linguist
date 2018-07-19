package require Tcl 8.5
package require Tk 8.5

wm attributes . -fullscreen 1
pack [canvas .c -highlightthick 0] -fill both -expand 1

# Add more values into this to do more greyscale bar variations
set splits {8 16 32 64}
set dy [expr {[winfo screenheight .c] / [llength $splits]}]
set y 0
foreach s $splits {
    set dx [expr {double([winfo screenwidth .c]) / $s}]
    set dc [expr {double(0xFF) / ($s-1)}]
    for {set i 0} {$i < $s} {incr i} {
	set c [expr {int($i * $dc)}]
	set x [expr {int($i * $dx)}]
	.c create rectangle $x $y [expr {$x+$dx+1}] [expr {$y+$dy+1}] \
            -fill [format "#%02x%02x%02x" $c $c $c] -outline {}
    }
    incr y $dy
}
