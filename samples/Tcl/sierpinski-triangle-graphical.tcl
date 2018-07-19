package require Tcl 8.5
package require Tk

proc mean args {expr {[::tcl::mathop::+ {*}$args] / [llength $args]}}
proc sierpinski {canv coords order} {
    $canv create poly $coords -fill black -outline {}
    set queue [list [list {*}$coords $order]]
    while {[llength $queue]} {
	lassign [lindex $queue 0] x1 y1 x2 y2 x3 y3 order
	set queue [lrange $queue 1 end]
	if {[incr order -1] < 0} continue
	set x12 [mean $x1 $x2]; set y12 [mean $y1 $y2]
	set x23 [mean $x2 $x3]; set y23 [mean $y2 $y3]
	set x31 [mean $x3 $x1]; set y31 [mean $y3 $y1]
	$canv create poly $x12 $y12 $x23 $y23 $x31 $y31 -fill white -outline {}
	update idletasks;	# So we can see progress
	lappend queue [list $x1 $y1 $x12 $y12 $x31 $y31 $order] \
	    [list $x12 $y12 $x2 $y2 $x23 $y23 $order] \
	    [list $x31 $y31 $x23 $y23 $x3 $y3 $order]
    }
}

pack [canvas .c -width 400 -height 400 -background white]
update;				# So we can see progress
sierpinski .c {200 10 390 390 10 390} 7
