package require Tk 8.6
package require mkLibsdl

# This code assumes we're dealing with the first pair of axes on the first
# joystick; modern joysticks are complex...

# Callback for all joystick activity
proc display {joyDict} {
    global x y buttons message
    set axis -1
    dict with joyDict {
	if {$joystick != 0} return
	if {[info exist button]} {
	    # Handle button presses...
	    set buttons($button) $value
	    set message "Buttons:"
	    foreach b [lsort -integer [array names buttons]] {
		if {$buttons($b)} {
		    lappend message $b
		}
	    }
	} else {
	    # Handle joystick movement...
	    if {$axis == -1} return
	    set value [expr {$value / 32768.0 * 100 + 120}]
	    if {$axis == 0} {
		set x $value
	    } elseif {$axis == 1} {
		set y $value
	    }
	    .c coords xhairV $x [expr {$y-5}] $x [expr {$y+5}]
	    .c coords xhairH [expr {$x-5}] $y [expr {$x+5}] $y
	}
    }
}

# Make a GUI
set message "Buttons:"
pack [canvas .c -width 240 -height 240] [label .l -textvariable message]
set x [set y 120]
.c create line {120 115 120 125} -tags xhairV
.c create line {115 120 125 120} -tags xhairH
joystick event eval {display [joystick event peek]}
