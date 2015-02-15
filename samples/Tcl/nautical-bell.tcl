# More sophisticated versions are possible, such as playing a bell sample
# using the Snack library.
proc ringTheBell {} {
    puts -nonewline "\a"
}

# The code to convert the (parsed) time into rings of the ship's bell and
# printing of the name of the bell.
proc strikeBell {hour minute} {
    global suppressNormalOutput
    set watches {
	Middle Middle Morning Morning Forenoon Forenoon
	Afternoon Afternoon {First dog} {Last dog} First First
    }
    set cardinals {one two three four five six seven eight}
    set bells [expr {(($hour % 4) * 2 + $minute / 30)}]
    if {!$bells} {set bells 8}
    puts -nonewline [format "%02d:%02d %9s watch, %6s bell%s gone: \t" \
	    $hour $minute [lindex $watches [expr {
		($hour/2 - ($minute==0 && $hour%2==0)) % 12
	    }]] [lindex $cardinals [expr {$bells - 1}]] \
	    [expr {$bells == 1 ? "" : "s"}]]

    # Set up the ringing of the bells to be done asynchronously
    set t 0
    set suppressNormalOutput 1
    for {set i 0} {$i < $bells-1} {incr i 2} {
	after $t {
	    ringTheBell
	    puts -nonewline "\u266b "
	}
	incr t 250
	after $t {
	    ringTheBell
	}
	incr t 750
    }
    if {$bells % 2} {
	after $t {
	    ringTheBell
	    puts -nonewline "\u266a\n"
	    set suppressNormalOutput 0
	}
    } else {
	after $t {
	    puts ""
	    set suppressNormalOutput 0
	}
    }
}

# Main handler; designed to be called every second, which is plenty.
proc nauticalBell {} {
    global last suppressNormalOutput
    scan [clock format [clock seconds] -format "%H:%M" -gmt 1] "%d:%d" h m
    if {$last != $m} {
	set last $m
	if {$m%30 == 0} {
	    strikeBell $h $m
	} elseif {!$suppressNormalOutput} {
	    puts -nonewline [format "%02d:%02d\r" $h $m]
	}
    }
}

# Set things up, using Tcl's event loop to do the processing
proc every {delay script} {
    after $delay [list every $delay $script]
    uplevel #0 $script
}
set last ""
set suppressNormalOutput 0
fconfigure stdout -buffering none
every 1000 nauticalBell
vwait forever;   # Only needed if not running an event loop otherwise
