package require Tcl 8.5
fconfigure stdout -buffering none

# Set up the grid and fill it with some mines
proc makeGrid {n m} {
    global grid mine
    unset -nocomplain grid mine
    set grid(size) [list $n $m]
    set grid(shown) 0
    set grid(marked) 0
    for {set j 1} {$j <= $m} {incr j} {
	for {set i 1} {$i <= $n} {incr i} {
	    set grid($i,$j) .
	}
    }
    set squares [expr {$m * $n}]
    set mine(count) [expr {int((rand()*0.4+0.2) * $squares)}]
    for {set count 0} {$count < $mine(count)} {incr count} {
	while 1 {
	    set i [expr {1+int(rand()*$n)}]
	    set j [expr {1+int(rand()*$m)}]
	    if {![info exist mine($i,$j)]} {
		set mine($i,$j) x
		break
	    }
	}
    }
    return $mine(count)
}

# Print out the grid
proc displayGrid {} {
    global grid
    lassign $grid(size) n m
    for {set j 1} {$j <= $m} {incr j} {
	set row "\t"
	for {set i 1} {$i <= $n} {incr i} {
	    append row $grid($i,$j)
	}
	puts $row
    }
}

# Toggle the possible-mine flag on a cell
proc markCell {x y} {
    global grid
    if {![info exist grid($x,$y)]} return
    if {$grid($x,$y) eq "."} {
	set grid($x,$y) "?"
	incr grid(marked)
    } elseif {$grid($x,$y) eq "?"} {
	set grid($x,$y) "."
	incr grid(marked) -1
    }
}

# Helper procedure that iterates over the 9 squares centered on a location
proc foreachAround {x y xv yv script} {
    global grid
    upvar 1 $xv i $yv j
    foreach i [list [expr {$x-1}] $x [expr {$x+1}]] {
	foreach j [list [expr {$y-1}] $y [expr {$y+1}]] {
	    if {[info exist grid($i,$j)]} {uplevel 1 $script}
	}
    }
}

# Reveal a cell; returns if it was a mine
proc clearCell {x y} {
    global grid mine
    if {![info exist grid($x,$y)] || $grid($x,$y) ne "."} {
	return 0; # Do nothing...
    }
    if {[info exist mine($x,$y)]} {
	set grid($x,$y) "!"
	revealGrid
	return 1; # Lose...
    }
    set surround 0
    foreachAround $x $y i j {incr surround [info exist mine($i,$j)]}
    incr grid(shown)
    if {$surround == 0} {
	set grid($x,$y) " "
	foreachAround $x $y i j {clearCell $i $j}
    } else {
	set grid($x,$y) $surround
    }
    return 0
}

# Check the winning condition
proc won? {} {
    global grid mine
    lassign $grid(size) n m
    expr {$grid(shown) + $mine(count) == $n * $m}
}

# Update the grid to show mine locations (marked or otherwise)
proc revealGrid {} {
    global grid mine
    lassign $grid(size) n m
    for {set j 1} {$j <= $m} {incr j} {
	for {set i 1} {$i <= $n} {incr i} {
	    if {![info exist mine($i,$j)]} continue
	    if {$grid($i,$j) eq "."} {
		set grid($i,$j) "x"
	    } elseif {$grid($i,$j) eq "?"} {
		set grid($i,$j) "X"
	    }
	}
    }
}

# The main game loop
proc play {n m} {
    set m [makeGrid $n $m]
    puts "There are $m true mines of fixed position in the grid\n"
    displayGrid
    while 1 {
	puts -nonewline "m x y/c x y/p/r: "
	if {[gets stdin line] < 0} break; # check for eof too!
	switch -nocase -regexp [string trim $line] {
	    {^m\s+\d+\s+\d+$} {
		markCell [lindex $line 1] [lindex $line 2]
	    }
	    {^c\s+\d+\s+\d+$} {
		if {[clearCell [lindex $line 1] [lindex $line 2]]} {
		    puts "KABOOM!"
		    displayGrid
		    break
		} elseif {[won?]} {
		    puts "You win!"
		    displayGrid
		    break
		}
	    }
	    {^p$} {
		displayGrid
	    }
	    {^r$} {
		puts "Resigning..."
		revealGrid
		displayGrid
		break
	    }
	}
    }
}

play 6 4
