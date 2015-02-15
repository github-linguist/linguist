package require Tcl 8.5

proc walk {y x} {
    global w ww h cnt grid len
    if {!$y || $y==$h || !$x || $x==$w} {
	incr cnt 2
	return
    }
    set t [expr {$y*$ww + $x}]
    set m [expr {$len - $t}]
    lset grid $t [expr {[lindex $grid $t] + 1}]
    lset grid $m [expr {[lindex $grid $m] + 1}]
    if {![lindex $grid [expr {$y*$ww + $x-1}]]} {
	walk $y [expr {$x-1}]
    }
    if {![lindex $grid [expr {($y-1)*$ww + $x}]]} {
	walk [expr {$y-1}] $x
    }
    if {![lindex $grid [expr {$y*$ww + $x+1}]]} {
	walk $y [expr {$x+1}]
    }
    if {![lindex $grid [expr {($y+1)*$ww + $x}]]} {
	walk [expr {$y+1}] $x
    }
    lset grid $t [expr {[lindex $grid $t] - 1}]
    lset grid $m [expr {[lindex $grid $m] - 1}]
}

# Factored out core of [solve]
proc SolveCore {} {
    global w ww h cnt grid len
    set ww [expr {$w+1}]
    set cy [expr {$h / 2}]
    set cx [expr {$w / 2}]

    set len [expr {($h+1) * $ww}]
    set grid [lrepeat $len 0]
    incr len -1

    for {set x $cx;incr x} {$x < $w} {incr x} {
	set t [expr {$cy*$ww+$x}]
	lset grid $t 1
	lset grid [expr {$len - $t}] 1
	walk [expr {$cy - 1}] $x
    }
    incr cnt
}
proc solve {H W} {
    global w h cnt
    set h $H
    set w $W
    if {$h & 1} {
	set h $W
	set w $H
    }
    if {$h & 1} {
	return 0
    }
    if {$w==1} {return 1}
    if {$w==2} {return $h}
    if {$h==2} {return $w}

    set cnt 0
    SolveCore
    if {$h==$w} {
	incr cnt $cnt
    } elseif {!($w & 1)} {
	lassign [list $w $h] h w
	SolveCore
    }
    return $cnt
}

apply {{limit} {
    for {set yy 1} {$yy <= $limit} {incr yy} {
	for {set xx 1} {$xx <= $yy} {incr xx} {
	    if {!($xx&1 && $yy&1)} {
		puts [format "%d x %d: %ld" $yy $xx [solve $yy $xx]]
	    }
	}
    }
}} 10
