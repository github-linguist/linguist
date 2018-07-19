package require Tk
proc r to {expr {int(rand()*$to)}};   # Simple helper

proc voronoi {photo pointCount} {
    for {set i 0} {$i < $pointCount} {incr i} {
	lappend points [r [image width $photo]] [r [image height $photo]]
    }
    foreach {x y} $points {
	lappend colors [format "#%02x%02x%02x" [r 256] [r 256] [r 256]]
    }
    set initd [expr {[image width $photo] + [image height $photo]}]
    for {set i 0} {$i < [image width $photo]} {incr i} {
	for {set j 0} {$j < [image height $photo]} {incr j} {
	    set color black
	    set d $initd
	    foreach {x y} $points c $colors {
		set h [expr {hypot($x-$i,$y-$j)}]
		### Other interesting metrics
		#set h [expr {abs($x-$i)+abs($y-$j)}]
		#set h [expr {(abs($x-$i)**3+abs($y-$j)**3)**0.3}]
		if {$d > $h} {set d $h;set color $c}
	    }
	    $photo put $color -to $i $j
	}
	# To display while generating, uncomment this line and the other one so commented
	#if {$i%4==0} {update idletasks}
    }
}

# Generate a 600x400 Voronoi diagram with 60 random points
image create photo demo -width 600 -height 400
pack [label .l -image demo]
# To display while generating, uncomment this line and the other one so commented
#update
voronoi demo 60
