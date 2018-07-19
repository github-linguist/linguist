package require Tcl 8.5
package require Tk 8.5

wm attributes . -fullscreen 1
pack [canvas .c -highlightthick 0] -fill both -expand 1
set colors {black red green blue magenta cyan yellow white}

set dy [expr {[winfo screenheight .c]/4}]
set y 0
foreach dx {1 2 3 4} {
    for {set x 0} {$x < [winfo screenwidth .c]} {incr x $dx} {
	.c create rectangle $x $y [expr {$x+$dx}] [expr {$y+$dy}] \
            -fill [lindex $colors 0] -outline {}
	set colors [list {*}[lrange $colors 1 end] [lindex $colors 0]]
    }
    incr y $dy
}
