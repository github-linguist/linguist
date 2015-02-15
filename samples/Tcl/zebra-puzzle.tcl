package require struct::list

# Implements the constants by binding them directly into the named procedures.
# This is much faster than the alternatives!
proc initConstants {args} {
    global {}
    set remap {}
    foreach {class elems} {
	Number {One Two Three Four Five}
	Color {Red Green Blue White Yellow}
	Drink {Milk Coffee Water Beer Tea}
	Smoke {PallMall Dunhill Blend BlueMaster Prince}
	Pet {Dog Cat Horse Bird Zebra}
	Nation {British Swedish Danish Norwegian German}
    } {
	set i -1
	foreach e $elems {lappend remap "\$${class}($e)" [incr i]}
	set ($class) $elems
    }
    foreach procedure $args {
	proc $procedure [info args $procedure] \
	    [string map $remap [info body $procedure]]
    }
}

proc isPossible {number color drink smoke pet} {
    if {[llength $number] && [lindex $number $Nation(Norwegian)] != $Number(One)} {
	return false
    } elseif {[llength $color] && [lindex $color $Nation(British)] != $Color(Red)} {
	return false
    } elseif {[llength $drink] && [lindex $drink $Nation(Danish)] != $Drink(Tea)} {
	return false
    } elseif {[llength $smoke] && [lindex $smoke $Nation(German)] != $Smoke(Prince)} {
	return false
    } elseif {[llength $pet] && [lindex $pet $Nation(Swedish)] != $Pet(Dog)} {
	return false
    }

    if {!([llength $number] && [llength $color] && [llength $drink] && [llength $smoke] && [llength $pet])} {
	return true
    }

    for {set i 0} {$i < 5} {incr i} {
	if {[lindex $color $i] == $Color(Green) && [lindex $drink $i] != $Drink(Coffee)} {
	    return false
	} elseif {[lindex $smoke $i] == $Smoke(PallMall) && [lindex $pet $i] != $Pet(Bird)} {
	    return false
	} elseif {[lindex $color $i] == $Color(Yellow) && [lindex $smoke $i] != $Smoke(Dunhill)} {
	    return false
	} elseif {[lindex $number $i] == $Number(Three) && [lindex $drink $i] != $Drink(Milk)} {
	    return false
	} elseif {[lindex $smoke $i] == $Smoke(BlueMaster) && [lindex $drink $i] != $Drink(Beer)} {
	    return false
	} elseif {[lindex $color $i] == $Color(Blue) && [lindex $number $i] != $Number(Two)} {
	    return false
	}

	for {set j 0} {$j < 5} {incr j} {
	    if {[lindex $color $i] == $Color(Green) && [lindex $color $j] == $Color(White) && [lindex $number $j] - [lindex $number $i] != 1} {
		return false
	    }

	    set diff [expr {abs([lindex $number $i] - [lindex $number $j])}]
	    if {[lindex $smoke $i] == $Smoke(Blend) && [lindex $pet $j] == $Pet(Cat) && $diff != 1} {
		return false
	    } elseif {[lindex $pet $i] == $Pet(Horse) && [lindex $smoke $j] == $Smoke(Dunhill) && $diff != 1} {
		return false
	    } elseif {[lindex $smoke $i] == $Smoke(Blend) && [lindex $drink $j] == $Drink(Water) && $diff != 1} {
		return false
	    }
	}
    }

    return true
}

proc showRow {t data} {
    upvar #0 ($t) elems
    puts [format "%6s: %12s%12s%12s%12s%12s" $t \
	      [lindex $elems [lindex $data 0]] \
	      [lindex $elems [lindex $data 1]] \
	      [lindex $elems [lindex $data 2]] \
	      [lindex $elems [lindex $data 3]] \
	      [lindex $elems [lindex $data 4]]]
}

proc main {} {
    set perms [struct::list permutations {0 1 2 3 4}]
    foreach number $perms {
	if {![isPossible $number {} {} {} {}]} continue
	foreach color $perms {
	    if {![isPossible $number $color {} {} {}]} continue
	    foreach drink $perms {
		if {![isPossible $number $color $drink {} {}]} continue
		foreach smoke $perms {
		    if {![isPossible $number $color $drink $smoke {}]} continue
		    foreach pet $perms {
			if {[isPossible $number $color $drink $smoke $pet]} {
			    puts "Found a solution:"
			    showRow Nation {0 1 2 3 4}
			    showRow Number $number
			    showRow Color  $color
			    showRow Drink  $drink
			    showRow Smoke  $smoke
			    showRow Pet    $pet
			    puts ""
			}
		    }
		}
	    }
	}
    }
}

initConstants isPossible
main
