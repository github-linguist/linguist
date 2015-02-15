oo::define maze {
    method solve {} {
	### Initialization of visited matrix and location/path queue
	set visited [lrepeat $x [lrepeat $y 0]]
	set queue {0 0 {}}

	### Loop to do the searching ###
	while 1 {
	    # Check for running out of path; an error in maze construction
	    if {[llength $queue] == 0} {
		error "cannot reach finish"
	    }
	    # Visit the next square from the queue
	    set queue [lassign $queue cx cy path]
	    if {[lindex $visited $cx $cy]} continue
	    lset visited $cx $cy 1
	    lappend path $cx $cy
	    # Check for reaching the goal
	    if {$cx == $x-1 && $cy == $y-1} break
	    # Add the square in each direction to the queue if a move there is legal
	    foreach {dx dy} {0 1  1 0  0 -1  -1 0} {
		set nx [expr {$cx + $dx}]; set ny [expr {$cy + $dy}]
		if {
		    $nx >= 0 && $nx < $x && $ny >= 0 && $ny < $y
		    && ($dx && idx($verti, min($cx,$nx), $cy) ||
			$dy && idx($horiz, $cx, min($cy,$ny)))
		} then {
		    lappend queue $nx $ny $path
		}
	    }
	}

	### Loop to set up the path rendering ###
	# (-2,-2) is just a marker that isn't next to the maze at all, so
	# guaranteeing the use of the last 'else' clause
	foreach {cx cy} $path {nx ny} [concat [lrange $path 2 end] -2 -2] {
	    if {$nx-$cx == 1} {
		lset content $cx $cy "v"
	    } elseif {$nx-$cx == -1} {
		lset content $cx $cy "^"
	    } elseif {$ny-$cy == -1} {
		lset content $cx $cy "<"
	    } else {
		lset content $cx $cy ">"
	    }
	}

	### Return the path ###
	return $path
    }
}

# Do the solution (we ignore the returned path here...)
m solve
# Print it out
puts [m view]
