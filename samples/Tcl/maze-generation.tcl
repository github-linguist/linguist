package require TclOO; # Or Tcl 8.6

# Helper to pick a random number
proc rand n {expr {int(rand() * $n)}}
# Helper to pick a random element of a list
proc pick list {lindex $list [rand [llength $list]]}
# Helper _function_ to index into a list of lists
proc tcl::mathfunc::idx {v x y} {lindex $v $x $y}

oo::class create maze {
    variable x y horiz verti content
    constructor {width height} {
	set y $width
	set x $height

	set n [expr {$x * $y - 1}]
	if {$n < 0} {error "illegal maze dimensions"}
	set horiz [set verti [lrepeat $x [lrepeat $y 0]]]
	# This matrix holds the output for the Maze Solving task; not used for generation
	set content [lrepeat $x [lrepeat $y " "]]
	set unvisited [lrepeat [expr {$x+2}] [lrepeat [expr {$y+2}] 0]]
	# Helper to write into a list of lists (with offsets)
	proc unvisited= {x y value} {
	    upvar 1 unvisited u
	    lset u [expr {$x+1}] [expr {$y+1}] $value
	}

	lappend stack [set here [list [rand $x] [rand $y]]]
	for {set j 0} {$j < $x} {incr j} {
	    for {set k 0} {$k < $y} {incr k} {
		unvisited= $j $k [expr {$here ne [list $j $k]}]
	    }
	}

	while {0 < $n} {
	    lassign $here hx hy
	    set neighbours {}
	    foreach {dx dy} {1 0  0 1  -1 0	 0 -1} {
		if {idx($unvisited, $hx+$dx+1, $hy+$dy+1)} {
		    lappend neighbours [list [expr {$hx+$dx}] [expr {$hy+$dy}]]
		}
	    }
	    if {[llength $neighbours]} {
		lassign [set here [pick $neighbours]] nx ny
		unvisited= $nx $ny 0
		if {$nx == $hx} {
		    lset horiz $nx [expr {min($ny, $hy)}] 1
		} else {
		    lset verti [expr {min($nx, $hx)}] $ny 1
		}
		lappend stack $here
		incr n -1
	    } else {
		set here [lindex $stack end]
		set stack [lrange $stack 0 end-1]
	    }
	}

	rename unvisited= {}
    }

    # Maze displayer; takes a maze dictionary, returns a string
    method view {} {
	set text {}
	for {set j 0} {$j < $x*2+1} {incr j} {
	    set line {}
	    for {set k 0} {$k < $y*4+1} {incr k} {
		if {$j%2 && $k%4==2} {
		    # At the centre of the cell, put the "content" of the cell
		    append line [expr {idx($content, $j/2, $k/4)}]
		} elseif {$j%2 && ($k%4 || $k && idx($horiz, $j/2, $k/4-1))} {
		    append line " "
		} elseif {$j%2} {
		    append line "|"
		} elseif {0 == $k%4} {
		    append line "+"
		} elseif {$j && idx($verti, $j/2-1, $k/4)} {
		    append line " "
		} else {
		    append line "-"
		}
	    }
	    if {!$j} {
		lappend text [string replace $line 1 3 "   "]
	    } elseif {$x*2-1 == $j} {
		lappend text [string replace $line end end " "]
	    } else {
		lappend text $line
	    }
	}
	return [join $text \n]
    }
}

# Demonstration
maze create m 11 8
puts [m view]
