package require Tcl 8.5
package require Tk

set SIZE 300

image create photo brownianTree -width $SIZE -height $SIZE
interp alias {} plot {} brownianTree put white -to
brownianTree put black -to 0 0 [expr {$SIZE-1}] [expr {$SIZE-1}]
proc rnd {range} {expr {int(rand() * $range)}}

proc makeBrownianTree count {
    global SIZE
    # Set the seed
    plot [rnd $SIZE] [rnd $SIZE]
    for {set i 0} {$i<$count} {incr i} {
	# Set a random particle's initial position
	set px [rnd $SIZE]
	set py [rnd $SIZE]

	while 1 {
	    # Randomly choose a direction
	    set dx [expr {[rnd 3] - 1}]
	    set dy [expr {[rnd 3] - 1}]

	    # If we are going out of bounds...
	    if {$px+$dx < 0 || $px+$dx >= $SIZE || $py+$dy < 0 || $py+$dy>=$SIZE} {
		# Out of bounds, so move back in
		set dx [expr {[rnd 3] - 1}]
		set dy [expr {[rnd 3] - 1}]
		continue
	    }

	    set ox $px
	    set oy $py
	    # Move/see if we would hit anything
	    incr px $dx
	    incr py $dy
	    if {[lindex [brownianTree get $px $py] 0]} {
		# Hit something, so plot where we were
		plot $ox $oy
		break
	    }
	}
	## For display while things are processing, uncomment next line
	#update;puts -nonewline .;flush stdout
    }
}

pack [label .l -image brownianTree]
update
makeBrownianTree 1000
brownianTree write tree.ppm
