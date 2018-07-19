package require Tk

set pi [expr acos(-1)]
set r2 [expr sqrt(2)]

proc turn {degrees} {
    global a pi
    set a [expr {$a + $degrees*$pi/180}]
}
proc forward {len} {
    global a coords
    lassign [lrange $coords end-1 end] x y
    lappend coords [expr {$x + cos($a)*$len}] [expr {$y + sin($a)*$len}]
}
proc dragon {len split {d 1}} {
    global r2 coords
    if {$split == 0} {
	forward $len
	return
    }

    # This next part is only necessary to allow the illustration of progress
    if {$split == 10 && [llength $::coords]>2} {
	.c coords dragon $::coords
	update
    }

    incr split -1
    set sublen [expr {$len/$r2}]
    turn [expr {$d*45}]
    dragon $sublen $split 1
    turn [expr {$d*-90}]
    dragon $sublen $split -1
    turn [expr {$d*45}]
}

set coords {150 180}
set a 0.0
pack [canvas .c -width 700 -height 500]
.c create line {0 0 0 0} -tag dragon
dragon 400 17
.c coords dragon $coords
