package require Tcl 8.5

# Build a grid
proc makeGrid {w h {treeProbability 0.5}} {
    global grid gridW gridH
    set gridW $w
    set gridH $h
    set grid [lrepeat $h [lrepeat $w " "]]
    for {set x 0} {$x < $w} {incr x} {
	for {set y 0} {$y < $h} {incr y} {
	    if {rand() < $treeProbability} {
		lset grid $y $x "#"
	    }
	}
    }
}

# Evolve the grid (builds a copy, then overwrites)
proc evolveGrid {{fireProbability 0.01} {plantProbability 0.05}} {
    global grid gridW gridH
    set newGrid {}
    for {set y 0} {$y < $gridH} {incr y} {
	set row {}
	for {set x 0} {$x < $gridW} {incr x} {
	    switch -exact -- [set s [lindex $grid $y $x]] {
		" " {
		    if {rand() < $plantProbability} {
			set s "#"
		    }
		}
		"#" {
		    if {[burningNeighbour? $x $y] || rand() < $fireProbability} {
			set s "o"
		    }
		}
		"o" {
		    set s " "
		}
	    }
	    lappend row $s
	}
	lappend newGrid $row
    }
    set grid $newGrid
}

# We supply the neighbourhood model as an optional parameter (not used...)
proc burningNeighbour? {
    x y
    {neighbourhoodModel {-1 -1  -1 0  -1 1  0 -1  0 1  1 -1  1 0  1 1}}
} {
    global grid gridW gridH
    foreach {dx dy} $neighbourhoodModel {
	set i [expr {$x + $dx}]
	if {$i < 0 || $i >= $gridW} continue
	set j [expr {$y + $dy}]
	if {$j < 0 || $j >= $gridH} continue
	if {[lindex $grid $j $i] eq "o"} {
	    return 1
	}
    }
    return 0
}

proc printGrid {} {
    global grid
    foreach row $grid {
	puts [join $row ""]
    }
}

# Simple main loop; press Return for the next step or send an EOF to stop
makeGrid 70 8
while 1 {
    evolveGrid
    printGrid
    if {[gets stdin line] < 0} break
}
