package require Tk

proc step {workarea} {
    global x y dir
    if {[lindex [$workarea get $x $y] 0]} {
	$workarea put black -to $x $y
	if {[incr dir] > 3} {set dir 0}
    } else {
	$workarea put white -to $x $y
	if {[incr dir -1] < 0} {set dir 3}
    }
    switch $dir {
	0 {incr x}
	1 {incr y}
	2 {incr x -1}
	3 {incr y -1}
    }
    expr {$x < 0 || $x >= [image width $workarea] || $y < 0 || $y >= [image height $workarea]}
}

image create photo antgrid -width 100 -height 100
pack [label .l -image antgrid]
antgrid put white -to 0 0 99 99
set x [set y 50]
set dir 0

while 1 {
    update
    if {[step antgrid]} break
}

# Produce output in file
antgrid write ant.gif -format gif
