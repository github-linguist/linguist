package require Tcl 8.6

proc examine {filename} {
    global cmds
    set RE "(?:^|\[\[\{\])\[\\w:.\]+"
    set f [open $filename]
    while {[gets $f line] >= 0} {
	set line [string trim $line]
	if {$line eq "" || [string match "#*" $line]} {
	    continue
	}
	foreach cmd [regexp -all -inline $RE $line] {
	    incr cmds([string trim $cmd "\{\["])
	}
    }
    close $f
}

# Parse each file on the command line
foreach filename $argv {
    examine $filename
}
# Get the command list in order of frequency
set cmdinfo [lsort -stride 2 -index 1 -integer -decreasing [array get cmds]]
# Print the top 10 (two list items per entry, so 0-19, not 0-9)
foreach {cmd count} [lrange $cmdinfo 0 19] {
    puts [format "%-20s%d" $cmd $count]
}
