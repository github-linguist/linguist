package require Tcl 8.5

for {set i 1} {$i <= 12} {incr i} {
    dict set hailenv hail$i [dict create num $i steps 0]
}
while 1 {
    set loopagain false
    foreach k [dict keys $hailenv] {
	dict with hailenv $k {
	    puts -nonewline [format %4d $num]
	    if {$num == 1} {
		continue
	    } elseif {$num & 1} {
		set num [expr {3*$num + 1}]
	    } else {
		set num [expr {$num / 2}]
	    }
	    set loopagain true
	    incr steps
	}
    }
    puts ""
    if {!$loopagain} break
}
puts "Counts..."
foreach k [dict keys $hailenv] {
    dict with hailenv $k {
	puts -nonewline [format %4d $steps]
    }
}
puts ""
