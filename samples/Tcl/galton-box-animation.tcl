package require Tcl 8.6

oo::class create GaltonBox {
    variable b w h n x y cnt step dropping

    constructor {BALLS {NUMPEGS 5} {HEIGHT 24}} {
	set n $NUMPEGS
	set w [expr {$n*2 + 1}]
	set h $HEIGHT
	puts -nonewline "\033\[H\033\[J"
	set x [set y [lrepeat $BALLS 0]]
	set cnt 0
	set step 0
	set dropping 1

	set b [lrepeat $h [lrepeat $w " "]]
	for {set i 0} {$i < $n} {incr i} {
	    for {set j [expr {-$i}]} {$j <= $i} {incr j 2} {
		lset b [expr {2*$i+2}] [expr {$j+$w/2}] "*"
	    }
	}
    }

    method show {} {
	puts -nonewline "\033\[H"
	set oldrow {}
	foreach row $b {
	    foreach char $row oldchar $oldrow {
		if {$char ne "*"} {
		    puts -nonewline "$char "
		} elseif {$oldchar eq " "} {
		    puts -nonewline "\033\[32m*\033\[m "
		} else {
		    puts -nonewline "\033\[31m*\033\[m "
		}
	    }
	    set oldrow $row
	    puts ""
	}
    }

    method Move idx {
	set xx [lindex $x $idx]
	set yy [lindex $y $idx]
	set kill 0

	if {$yy < 0} {return 0}
	if {$yy == $h-1} {
	    lset y $idx -1
	    return 0
	}

	switch [lindex $b [incr yy] $xx] {
	    "*" {
		incr xx [expr {2*int(2 * rand()) - 1}]
		if {[lindex $b [incr yy -1] $xx] ne " "} {
		    set dropping 0
		}
	    }
	    "o" {
		incr yy -1
		set kill 1
	    }
	}

	set c [lindex $b [lindex $y $idx] [lindex $x $idx]]
	lset b [lindex $y $idx] [lindex $x $idx] " "
	lset b $yy $xx $c
	if {$kill} {
	    lset y $idx -1
	} else {
	    lset y $idx $yy
	}
	lset x $idx $xx
	return [expr {!$kill}]
    }

    method step {} {
	set moving 0
	for {set i 0} {$i < $cnt} {incr i} {
	    set moving [expr {[my Move $i] || $moving}]
	}
	if {2 == [incr step] && $cnt < [llength $x] && $dropping} {
	    set step 0
	    lset x $cnt [expr {$w / 2}]
	    lset y $cnt 0
	    if {[lindex $b [lindex $y $cnt] [lindex $x $cnt]] ne " "} {
		return 0
	    }
	    lset b [lindex $y $cnt] [lindex $x $cnt] "o"
	    incr cnt
	}
	return [expr {($moving || $dropping)}]
    }
}

GaltonBox create board 1024 {*}$argv
while true {
    board show
    if {[board step]} {after 60} break
}
